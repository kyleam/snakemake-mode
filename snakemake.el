;;; snakemake.el --- Call Snakemake from Emacs       -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/snakemake-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a popup interface, built on Magit's popup
;; library [1], for calling Snakemake [2].  The main entry point is
;; `snakemake-popup', which you should consider giving a global key
;; binding.
;;
;; The popup currently includes four actions, all which lead to
;; `compile' being called with "snakemake ...".  What's different
;; between the actions is how targets are selected.
;;
;; snakemake-build-targets-at-point
;;
;;     This command builds the file name or rule at point if it's a
;;     valid build target.
;;
;;     How the file name targets are retrieved depends on functions in
;;     `snakemake-file-targets-hook'.  The default functions grab file
;;     names from Dired, Org links, and general text under point that
;;     looks like a file name.
;;
;; snakemake-compile
;;
;;     This action leads to an interactive `compile' call that allows
;;     you to edit the command before it is run.
;;
;;     It tries to guess what the target name should be, but it
;;     doesn't verify if this target is actually a build target.  This
;;     lax behavior is useful to, for example, pull in an input file
;;     name from Dired and then edit the extension to get the desired
;;     output name.
;;
;; snakemake-build-rule-target
;;
;;     This command prompts with a list of all target rules for the
;;     current Snakefile and builds the selected one.
;;
;; snakemake-build-file-target
;;
;;     Like `snakemake-build-rule-target', but this command prompts
;;     for a file name, using the file name at point, if it's a valid
;;     target, as the default completion.
;;
;;     When `snakemake-file-target-program' is set to a program that
;;     outputs a list of files, the user is prompted with that list
;;     instead.  snakemake.el's source repo contains a script,
;;     "snakemake-file-targets", that can be used for this purpose.
;;     It outputs a list of all concrete output files for the current
;;     Snakefile.
;;
;; All these commands use the Snakefile contained in the
;; `default-directory' of the current buffer.  If
;; `snakemake-root-dir-function' is non-nil, this function should
;; return a secondary directory (e.g., a project repository root) in
;; which to look for a Snakefile if there isn't one in the current
;; directory.
;;
;; In addition to the popup commands, there are commands for showing
;; and saving the dependency graph of a target.  The command
;; `snakemake-graph' displays the graph in a buffer.  From this
;; buffer, `snakemake-graph-save' can be called to save the graph to
;; an output file.
;;
;; [1] http://magit.vc/manual/magit-popup.html
;; [2] https://bitbucket.org/snakemake/snakemake/wiki/Home

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'magit-popup)

(require 'snakemake-mode)


;;; Customization

;;;###autoload
(defgroup snakemake nil
  "Interface for running Snakemake"
  :group 'tools)

(defcustom snakemake-program "snakemake"
  "Command to run Snakemake."
  :type 'string
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-file-target-program
  (executable-find "snakemake-file-targets")
  "Program that returns newline-delimited list of output files."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Program"))
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-root-dir-function nil
  "Function used to find root directory of the current \"project\".
This function will be called with no arguments and should return
an absolute path or, if no root is found, nil.  When nil, only
Snakefiles in the current directory will be detected."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Use VC" vc-root-dir)
                 (function :tag "Use Projectile" projectile-project-root))
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-file-targets-hook
  '(snakemake-region-file-targets
    snakemake-dired-file-targets
    snakemake-org-link-file-targets
    snakemake-thingatpt-file-targets)
  "Functions to return file targets at point.
These will be called, with no arguments, until one of them
signals success by returning non-nil.  If non-nil, the return
value should be a list of absolute paths."
  :type 'hook
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-region-files-strip-re
  (concat (regexp-opt '("[" "]" "'" "\"" ";" ",")) "+")
  "Regexp matching text to be discarded when collecting region files.
Used by `snakemake-region-file-targets'."
  :type 'regexp
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-dot-program "dot"
  "Program used to save the graph with `snakemake-graph-save'.
This program must have an option '-T' that can be used to specify
the output type.  See 'man dot'."
  :type 'string
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-graph-default-extension ".svg"
  "Extension used by `snakemake-graph-save'.
This should be a valid value for the '-T' option of
`snakemake-dot-program'."
  :type 'string
  :package-version '(snakemake-mode . "0.4.0"))


;;; Utilities

(defun snakemake-snakefile-directory (&optional path)
  "Return current Snakefile's directory for PATH.

If PATH is nil, it defaults to `default-directory'.

Which Snakefile, if any, is current is determined by the value of
`default-directory'.

* A file named \"Snakefile\" in `default-directory' is always
  considered the current file.

* If `default-directory' does not contain a file named
  \"Snakefile\", look in the directory given by
  `snakemake-root-dir-function'."
  (let ((default-directory
          (or (and path (file-name-directory (expand-file-name path)))
              default-directory)))
    (or (and (file-exists-p "Snakefile") default-directory)
        (and snakemake-root-dir-function
             (let ((topdir (ignore-errors
                             (funcall snakemake-root-dir-function))))
               (when topdir
                 (and (file-exists-p (expand-file-name "Snakefile" topdir))
                      topdir))))
        (user-error "No Snakefile found for %s" default-directory))))

(defvar snakemake--cache (make-hash-table :test #'equal))
(defmacro snakemake-with-cache (directory cache-info &rest body)
  "Execute BODY and cache result, with DIRECTORY's Snakefile as current.
CACHE-INFO should uniquely identify the call when taken together
with DIRECTORY and the Snakefile's modification time."
  (declare (indent defun) (debug (form form body)))
  (let ((cached (cl-gensym "cached"))
        (key (cl-gensym "key")))
    `(let* ((default-directory (snakemake-snakefile-directory ,directory))
            (,key (list default-directory
                        (nth 5 (file-attributes
                                (expand-file-name "Snakefile")))
                        ,@cache-info))
            (,cached (gethash ,key snakemake--cache 'not-found)))
       (if (eq ,cached 'not-found)
           (let ((result ,(macroexp-progn body)))
             (puthash ,key result snakemake--cache)
             result)
         ,cached))))

(defun snakemake-insert-output (&rest args)
  "Call `snakemake-program' with ARGS and insert output."
  (apply #'call-process snakemake-program nil t nil args))

(defun snakemake--split-rules (type)
  "Return rules of TYPE.
TYPE can be `all' or `target'."
  (cl-remove-if
   (lambda (x) (string-match-p "\\`[0-9]+\\'" x))
   (split-string
    (with-temp-buffer
      (if (= 0 (snakemake-insert-output
                "--nocolor"
                (cl-case type
                  (all "--list")
                  (target "--list-target-rules")
                  (t (user-error "Invalid rule type: %s" type)))))
          (buffer-string)
        (error "Error finding rules"))))))

(defun snakemake-all-rules (&optional directory)
  "Return list of rules for DIRECTORY's Snakefile."
  (snakemake-with-cache directory ("all-rules")
    (snakemake--split-rules 'all)))

(defun snakemake-rule-targets (&optional directory)
  "Return list of target rules for DIRECTORY's Snakefile."
  (snakemake-with-cache directory ("target-rules")
    (snakemake--split-rules 'target)))

(defun snakemake-file-targets (&optional directory)
  "Return list of output files for DIRECTORY's Snakefile.
The file list is determined by the output of
`snakemake-file-target-program'."
  (when snakemake-file-target-program
    (snakemake-with-cache directory ("target-files")
      (split-string
       (with-temp-buffer
         (if (= 0 (call-process snakemake-file-target-program nil t))
             (buffer-string)
           (error "Error finding file targets")))))))

(defun snakemake-check-target (target &optional directory)
  "Return non-nil if TARGET is a valid target for DIRECTORY's Snakefile."
  (snakemake-with-cache directory (target)
    (with-temp-buffer
      (snakemake-insert-output "--quiet" "--dryrun" target)
      (goto-char (point-min))
      ;; Lean towards misclassifying targets as valid rather than
      ;; silently dropping valid targets as invalid.
      (not (looking-at (regexp-opt (list "MissingRuleException"
                                         "RuleException")))))))

(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(defun snakemake-org-link-file-targets ()
  "Return file path from Org link.
This function returns a list for consistency with other
target-returning functions, but any non-nil return value is
currently limited to a single-item list."
  (when (derived-mode-p 'org-mode)
    (let ((el (org-element-context)))
      ;; Don't use `org-element-lineage' because it isn't available
      ;; until Org version 8.3.
      (while (and el (not (memq (car el) '(link))))
        (setq el (org-element-property :parent el)))
      (when (eq (car el) 'link)
        (list (expand-file-name (org-element-property :path el)))))))

(declare-function dired-get-marked-files "dired")
(defun snakemake-dired-file-targets ()
  "Return marked Dired files."
  (and (derived-mode-p 'dired-mode)
       (dired-get-marked-files)))

(defun snakemake-region-file-targets (&optional beg end)
  "Return file targets in region.

Before generating the list, characters that match
`snakemake-region-files-strip-re' are discarded.

If BEG or END is non-nil, use them in place of `region-beginning'
or `region-end', respectively."
  (when (or (use-region-p) (and beg end))
    (mapcar #'expand-file-name
            (split-string
             (replace-regexp-in-string
              snakemake-region-files-strip-re " "
              (buffer-substring-no-properties (or beg (region-beginning))
                                              (or end (region-end))))))))

(defun snakemake-thingatpt-file-targets ()
  "Return file at point accordinng `thing-at-point'.
This function returns a list for consistency with other
target-returning functions, but any non-nil return value is
currently limited to a single-item list."
  (let ((fname (thing-at-point 'filename)))
    (and fname
         (list (expand-file-name fname)))))

(defun snakemake-file-targets-at-point (&optional check)
  "Return list of file targets at point.
If CHECK is non-nil, filter files to known targets of the current
Snakefile."
  (let* ((dir (snakemake-snakefile-directory))
         (fnames (run-hook-with-args-until-success
                  'snakemake-file-targets-hook))
         (targets (mapcar (lambda (f) (file-relative-name f dir))
                          fnames)))
    (when targets
      (if check
          (let ((default-directory dir))
            (cl-remove-if-not #'snakemake-check-target targets))
        targets))))

(defun snakemake-rule-at-point (&optional targets-only)
  "Return name of rule at point.

If TARGETS-ONLY is non-nil, verify that the rule is a valid
target.

This function returns a list for consistency with other
target-returning functions, but any non-nil return value is
currently limited to a single-item list."
  (when (derived-mode-p 'snakemake-mode)
    (let* ((info (snakemake-block-info))
           (rule (and (equal (nth 0 info) "rule")
                      (nth 1 info))))
      (when rule
        (and (or (not targets-only) (snakemake-check-target rule))
             (list rule))))))

(defun snakemake--prompt (prompt default)
  (concat prompt
          (and default (format " (default %s)" default))
          ": "))

(defun snakemake-read-file-target ()
  "Read a file target.
If `snakemake-file-target-program' is non-nil, use it to generate
a collection file targets to prompt with.  Otherwise, just read a
file name, adjusting the returned file name's path relative to
`snakemake-snakefile-directory'."
  (let ((default (car (snakemake-file-targets-at-point t))))
    (if snakemake-file-target-program
        (completing-read
         (snakemake--prompt "File" default)
         (snakemake-file-targets)
         nil nil nil nil
         default)
      (let* ((sdir (snakemake-snakefile-directory))
             (rel-default (and default
                               (file-relative-name
                                (expand-file-name default sdir)))))
        (file-relative-name
         (read-file-name
          (snakemake--prompt "File" rel-default)
          nil rel-default)
         sdir)))))

(defun snakemake-read-rule (&optional targets-only)
  "Read a rule for the current Snakefile.
If TARGETS-ONLY is non-nil, only prompt with rules that are valid
targets."
  (let ((default (car (snakemake-rule-at-point targets-only))))
    (completing-read
     (snakemake--prompt "Rule" default)
     (if targets-only (snakemake-rule-targets) (snakemake-all-rules))
     nil nil nil nil
     default)))

(defun snakemake-read-rules (&rest _)
  "Read multiple rules for the current Snakefile."
  (mapconcat #'identity
             (completing-read-multiple "Rules (comma-separated): "
                                       (snakemake-all-rules))
             " "))


;;; Graphing commands

(defvar-local snakemake-graph-id nil)

;;;###autoload
(defun snakemake-graph (rules &optional rule-graph)
  "Display graph for DAG of RULES.

The graph will be displayed with `image-mode'.  From this buffer,
you can call \\<snakemake-graph-mode-map>\\[snakemake-graph-save] \
to save the graph.

If prefix argument RULE-GRAPH is non-nil, pass --rulegraph
instead of --dag to snakemake.

$ snakemake --{dag,rulegraph} -- RULES | display"
  (interactive (list (or (snakemake-file-targets-at-point 'check)
                         (snakemake-rule-at-point 'target)
                         (snakemake-read-rule 'targets))
                     current-prefix-arg))
  (unless (listp rules)
    (setq rules (list rules)))
  (let ((dir (snakemake-snakefile-directory)))
    (with-current-buffer (get-buffer-create "*Snakemake graph*")
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process snakemake-program nil t nil
               (if rule-graph "--rulegraph" "--dag")
               rules))
      (image-mode)
      (snakemake-graph-mode)
      (setq snakemake-graph-id (mapconcat #'file-name-nondirectory rules "-"))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun snakemake-graph-this-file (&optional rule-graph directory)
  "Display graph of DAG for the first rule of the current file.

The graph will be displayed with `image-mode'.  From this buffer,
you can call \\<snakemake-graph-mode-map>\\[snakemake-graph-save] \
to save the graph.

If RULE-GRAPH is non-nil, pass --rulegraph instead of --dag to
the Snakemake call.  Snakemake is called from DIRECTORY or, if
DIRECTORY is nil, from the current file's directory.

Interactively, \\[universal-argument] indicates to use \
--rulegraph instead of --dag,
whereas \\[universal-argument] \\[universal-argument] signals to \
prompt user for a directory from which
to call Snakemake.  With any other non-nil prefix value, both of the
above actions are performed.

$ snakemake -s <current file> --{dag,rulegraph} | display"
  (interactive
   (let ((read-dir (lambda ()
                     (read-directory-name "Call from directory: "
                                          nil nil t))))
     (cond ((equal current-prefix-arg '(4))
            (list t nil))
           ((equal current-prefix-arg '(16))
            (list nil (funcall read-dir)))
           (current-prefix-arg
            (list t (funcall read-dir)))
           (t
            (list nil nil)))))
  (unless (derived-mode-p 'snakemake-mode)
    (user-error "Current buffer is not in Snakemake mode"))
  (let* ((file (or (buffer-file-name (buffer-base-buffer))
                   (user-error "Buffer is not visiting a file")))
         (dir (or directory (file-name-directory file)))
         ret-val)
    (with-current-buffer (get-buffer-create "*Snakemake graph*")
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq ret-val (call-process snakemake-program nil t nil
                                    (if rule-graph "--rulegraph" "--dag")
                                    "--snakefile" file)))
      (if (= 0 ret-val)
          (progn (image-mode)
                 (snakemake-graph-mode)
                 (setq snakemake-graph-id file))
        (goto-char (point-min))
        (insert (format "Error in snakemake call from %s:\n\n" dir)))
      (pop-to-buffer (current-buffer)))))

(defun snakemake-graph-save ()
  "Save graph in current buffer to file.
The graph will be processed by `snakemake-dot-program'.  The
default extension of the output file is
`snakemake-graph-default-extension', but you can enter any
extension that the dot program supports."
  (interactive)
  (unless snakemake-graph-id
    (user-error "Not in Snakemake graph buffer"))
  (let ((file (read-file-name "To file: " nil nil nil
                              (concat snakemake-graph-id
                                      snakemake-graph-default-extension))))
    (unless (or (string-match-p "\\`\\s-*\\'" file)
                (and (file-exists-p file)
                     (not (y-or-n-p
                           (concat file " already exists.  Overwrite?")))))
      (call-process-region (point-min) (point-max)
                           snakemake-dot-program nil (list :file file) nil
                           "-T" (file-name-extension file)))))

(defvar snakemake-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'snakemake-graph-save)
    map)
  "Keymap for Snakemake-graph mode.")

(define-minor-mode snakemake-graph-mode
  "Toggle Snakemake-graph mode.

With a prefix argument ARG, enable Snakemake-graph mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Snakemake-graph mode is a minor mode that provides a key,
\\<snakemake-graph-mode-map>\\[snakemake-graph-save], for saving the graph to an output file."
  :keymap snakemake-graph-mode-map)


;;; Compilation commands

(add-to-list 'compilation-error-regexp-alist 'snakemake)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(snakemake . ("^SyntaxError in line \\([0-9]+\\) of \\(.*[^A-z]Snakefile\\):$"
                2 1)))

(defun snakemake--define-compile-command (targets args)
  (mapconcat #'identity
             `(,snakemake-program ,@args "--" ,@targets)
             " "))

(defun snakemake-compile-targets (targets args)
  "Run non-interactive `compile' with 'snakemake [ARGS] -- TARGETS'."
  (let* ((default-directory (snakemake-snakefile-directory))
         (cmd (snakemake--define-compile-command targets args)))
    (compile cmd)
    (push cmd compile-history)))

;;;###autoload
(defun snakemake-build-targets-at-point (&optional args)
  "Build target(s) at point without any prompts.

$ snakemake [ARGS] -- <targets>"
  (interactive (list (snakemake-arguments)))
  (let ((targets (or (snakemake-file-targets-at-point 'check)
                     (snakemake-rule-at-point 'target)
                     (user-error "No target found at point")))
        (default-directory (snakemake-snakefile-directory)))
    (snakemake-compile-targets targets args)))

;;;###autoload
(defun snakemake-build-file-target (&optional args)
  "Build target file.

$ snakemake [ARGS] -- <file>"
  (interactive (list (snakemake-arguments)))
  (snakemake-compile-targets
   (list (snakemake-read-file-target))
   args))

;;;###autoload
(defun snakemake-build-rule-target (&optional args)
  "Build target rule, prompting with known rules.

$ snakemake [ARGS] -- <rule>"
  (interactive (list (snakemake-arguments)))
  (snakemake-compile-targets
   (list (snakemake-read-rule 'targets))
   args))

;;;###autoload
(defun snakemake-compile (&optional args)
  "Read `compile' command for building targets.

$ snakemake [ARGS] -- <targets>"
  (interactive (list (snakemake-arguments)))
  (let ((compile-command (snakemake--define-compile-command
                          (or (snakemake-file-targets-at-point)
                              (snakemake-rule-at-point)
                              (list ""))
                          args))
        (compilation-read-command t)
        (default-directory (snakemake-snakefile-directory)))
    (call-interactively #'compile)))

;;;###autoload (autoload 'snakemake-popup "snakemake" nil t)
(magit-define-popup snakemake-popup
  "Popup console for running Snakemake."
  :switches
  '((?f "Force" "--force")
    (?i "Ignore temp()" "--notemp")
    (?n "Dry run" "--dryrun")
    (?p "Print shell commands" "-p")
    (?r "Print reason" "--reason")
    (?t "Touch files" "--touch"))
  :options
  '((?a "Allowed rules" "--allowed-rules " snakemake-read-rules)
    (?j "Number of jobs" "-j"))
  :actions
  '((?c "Compile" snakemake-compile) nil nil
    (?p "Build target at point" snakemake-build-targets-at-point)
    (?f "Build file" snakemake-build-file-target)
    (?r "Build rule" snakemake-build-rule-target))
  :default-action 'snakemake-compile
  :max-action-columns 3)

(provide 'snakemake)
;;; snakemake.el ends here
