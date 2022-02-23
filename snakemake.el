;;; snakemake.el --- Call Snakemake from Emacs       -*- lexical-binding: t; -*-

;; Copyright all Snakemake mode contributors <snakemake-mode@inbox.kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://git.kyleam.com/snakemake-mode/about

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a Transient interface [1] for calling
;; Snakemake [2].  The main entry point is the `snakemake' command,
;; which you should consider giving a global key binding.
;;
;; The `snakemake' transient currently includes four actions, all
;; which lead to a Snakemake call.  What's different between the
;; actions is how targets are selected.
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
;; snakemake-build
;;
;;     This action allows you to edit the command before running it.
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
;; By default, commands are executed through `compile'.  However, a
;; terminal can be created with `snakemake-term-start', in which case
;; commands are sent there instead.
;;
;; In addition to the commands in the transient, there are commands
;; for showing and saving the dependency graph of a target.  The
;; command `snakemake-graph' displays the graph in a buffer.  From
;; this buffer, `snakemake-graph-save' can be called to save the graph
;; to an output file.
;;
;; [1] https://magit.vc/manual/transient/
;; [2] https://snakemake.github.io/

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'term)
(require 'transient)

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

(defcustom snakemake-shell-file-name shell-file-name
  "Program used by `ansi-term' to start a terminal."
  :type 'string
  :package-version '(snakemake-mode . "1.2.0"))

(defcustom snakemake-file-target-program
  (executable-find "snakemake-file-targets")
  "Program that returns newline-delimited list of output files."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Program"))
  :package-version '(snakemake-mode . "0.4.0"))

(defcustom snakemake-always-use-term nil
  "Whether commands should be sent to the terminal only.

When nil, commands are sent to the terminal if there is an active
terminal for the current Snakefile directory.  If there isn't an
active terminal, commands are executed through `compile'.

When this variable is non-nil, always try to send the command to
the terminal.  If no terminal is found, the command will be
aborted with a message telling you to first run
`\\[snakemake-term-start]'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(snakemake-mode . "1.2.0"))

(defcustom snakemake-root-dir-function nil
  "Function used to find root directory of the current \"project\".
This function will be called with no arguments and should return
an absolute path or, if no root is found, nil.  When nil, only
Snakefiles in the current directory will be detected."
  :type '(radio (const :tag "None" nil)
                (function-item vc-root-dir)
                (function-item projectile-project-root)
                function)
  :package-version '(snakemake-mode . "1.2.0"))

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
        (user-error "No Snakefile found for %s"
                    (abbreviate-file-name default-directory)))))

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

(define-error 'snakemake-error "Snakemake process error")

(defconst snakemake-error-buffer "*Snakemake process error*")

(defun snakemake--display-error ()
  (ignore-errors (kill-buffer snakemake-error-buffer))
  (let ((buf (get-buffer-create snakemake-error-buffer)))
    (copy-to-buffer buf (point-min) (point-max))
    (with-current-buffer buf (help-mode))
    (display-buffer buf)
    (signal 'snakemake-error nil)))

(defun snakemake-insert-output (&rest args)
  "Call `snakemake-program' with ARGS and insert output."
  (apply #'call-process snakemake-program nil t nil args))

(defun snakemake--split-lines (str &optional remove-num)
  (cl-remove-if
   (lambda (x) (or (and remove-num (string-match-p "\\`[0-9]+\\'" x))
                   ;; There is not a clean separation of stderr/stdout
                   ;; streams for warnings, so this is a hacky way to
                   ;; filter non-target lines from the output.
                   (string-match-p " " x)))
   (split-string str "\n" t)))

(defun snakemake--split-rules (type)
  "Return rules of TYPE.
TYPE can be `all' or `target'."
  (snakemake--split-lines
   (with-temp-buffer
     (if (zerop (snakemake-insert-output
                 "--nocolor"
                 (cl-case type
                   (all "--list")
                   (target "--list-target-rules")
                   (t (user-error "Invalid rule type: %s" type)))))
         (buffer-string)
       (snakemake--display-error)))
   t))

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
      (snakemake--split-lines
       (with-temp-buffer
         (if (zerop (call-process snakemake-file-target-program nil t))
             (buffer-string)
           (snakemake--display-error)))))))

(defconst snakemake-valid-target-re "ProtectedOutputException"
  "Regular expression indicating valid target.
If this matches, the target will be considered valid even if the
exit status is non-zero.")

(defun snakemake-check-target (target &optional directory)
  "Return non-nil if TARGET is a valid target for DIRECTORY's Snakefile."
  (snakemake-with-cache directory (target)
    (with-temp-buffer
      (let ((ex-code (snakemake-insert-output "--quiet" "--dryrun" target)))
        (or (zerop ex-code)
            (progn (goto-char (point-min))
                   (re-search-forward snakemake-valid-target-re nil t))
            ;; A non-zero exit status could indicate that TARGET is
            ;; invalid, but it could also be the result of an issue
            ;; like a syntax error or an ambiguous rule.  To check
            ;; this, see whether `snakemake-all-rules' signals a
            ;; `snakemake-error'.  This avoids relying on parsing
            ;; Snakemake exception output, which isn't stable across
            ;; Snakemake versions.
            (progn (snakemake-all-rules) nil))))))

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
  "Return file at point according to `thing-at-point'.
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
a collection of file targets to prompt with.  Otherwise, just
read a file name, adjusting the returned file name's path
relative to `snakemake-snakefile-directory'."
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

(defun snakemake-graph--display ()
  (require 'image)
  (if (not (image-type-available-p 'imagemagick))
      (find-file (snakemake-graph-save))
    (image-mode)
    (pop-to-buffer (current-buffer))))

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
      (snakemake-graph-mode)
      (setq snakemake-graph-id (mapconcat #'file-name-nondirectory rules "-"))
      (snakemake-graph--display))))

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
      (if (zerop ret-val)
          (progn (snakemake-graph-mode)
                 (setq snakemake-graph-id file)
                 (snakemake-graph--display))
        (goto-char (point-min))
        (insert (format "Error in snakemake call from %s:\n\n" dir))
        (pop-to-buffer (current-buffer))))))

(defun snakemake-graph-save ()
  "Save graph in current buffer to file.

The graph will be processed by `snakemake-dot-program'.  The
default extension of the output file is
`snakemake-graph-default-extension', but you can enter any
extension that the dot program supports.

Return the name of the output file."
  (interactive)
  (unless snakemake-graph-id
    (user-error "Not in Snakemake graph buffer"))
  (let ((file (read-file-name "To file: " nil nil nil
                              (concat snakemake-graph-id
                                      snakemake-graph-default-extension))))
    (cond
     ((string-match-p "\\`\\s-*\\'" file)
      (user-error "No output file specified"))
     ((and (file-exists-p file)
           (not (y-or-n-p
                 (concat file " already exists.  Overwrite?"))))
      (user-error "Aborted"))
     (t
      (call-process-region (point-min) (point-max)
                           snakemake-dot-program nil (list :file file) nil
                           "-T" (file-name-extension file))
      file))))

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


;;; Commands for calling Snakemake

(defun snakemake--make-command (targets args)
  (mapconcat #'identity
             `(,snakemake-program ,@args "--" ,@targets)
             " "))

;;;; Compilation interface

(add-to-list 'compilation-error-regexp-alist 'snakemake)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(snakemake . ("^SyntaxError in line \\([0-9]+\\) of \\(.*[^A-z]Snakefile\\):$"
                2 1)))

(defun snakemake-compile-targets (targets args)
  "Run non-interactive `compile' with 'snakemake [ARGS] -- TARGETS'."
  (let ((cmd (snakemake--make-command targets args)))
    (compile cmd)
    (push cmd compile-history)))

;;;; Terminal interface

(defun snakemake-term--name ()
  (concat "snakemake-terminal: " (abbreviate-file-name default-directory)))

(defun snakemake-term-process ()
  "Return the terminal process of the current directory."
  (get-process (concat "*" (snakemake-term--name) "*")))

(defun snakemake-term-use-p ()
  (or snakemake-always-use-term (snakemake-term-process)))

;;;###autoload
(defun snakemake-term-start ()
  "Start a terminal session for the current Snakefile directory.

The main advantage of using a terminal is that it allows for a
persistent environment between Snakemake calls, which is useful
for running Snakemake in isolated environments created by tools
like Guix.

To do so, `snakemake-shell-file-name' should be set to a script
that starts a shell with the desired environment.  For example,
to set up an enviroment with Guix, `snakemake-shell-file-name'
could point to a script that runs

    guix environment --pure -m manifest.scm --ad-hoc snakemake

The file 'guix-snakemake-environment' in Snakemake mode's source
directory can be used for this purpose."
  (interactive)
  (let ((default-directory (snakemake-snakefile-directory)))
    (unless (snakemake-term-process)
      (ansi-term snakemake-shell-file-name (snakemake-term--name)))))

(defun snakemake-term-send (string)
  "Send STRING to the terminal for the current directory."
  (let* ((proc (or (snakemake-term-process)
                   (user-error "No active terminal.  Start with %s"
                               (substitute-command-keys
                                "`\\[snakemake-term-start]'"))))
         (buf (process-buffer proc)))
    (unless (get-buffer-window buf)
      (display-buffer buf))
    (term-send-string proc (concat string "\n"))
    (with-current-buffer buf
      (goto-char (process-mark proc)))))

(defun snakemake-term-build-targets (targets args)
  "Send 'snakemake [ARGS] -- TARGETS' to the terminal."
  (snakemake-term-send (snakemake--make-command targets args)))

;;;; General interface

(defun snakemake-build-targets (targets args)
  "Run 'snakemake [ARGS] -- TARGETS'.
If a terminal is associated with the current Snakefile directory,
send the command there.  Otherwise, run the command with
`compile'."
  (let ((default-directory (snakemake-snakefile-directory)))
    (funcall (if (snakemake-term-use-p)
                 #'snakemake-term-build-targets
               #'snakemake-compile-targets)
             targets
             args)))

;;;###autoload
(defun snakemake-build-targets-at-point (&optional args)
  "Build target(s) at point without any prompts.

$ snakemake [ARGS] -- <targets>"
  (interactive (list (transient-args 'snakemake)))
  (snakemake-build-targets
   (or (snakemake-file-targets-at-point 'check)
       (snakemake-rule-at-point 'target)
       (user-error "No target found at point"))
   args))

;;;###autoload
(defun snakemake-build-file-target (&optional args)
  "Build target file.

$ snakemake [ARGS] -- <file>"
  (interactive (list (transient-args 'snakemake)))
  (snakemake-build-targets
   (list (snakemake-read-file-target))
   args))

;;;###autoload
(defun snakemake-build-rule-target (&optional args)
  "Build target rule, prompting with known rules.

$ snakemake [ARGS] -- <rule>"
  (interactive (list (transient-args 'snakemake)))
  (snakemake-build-targets
   (list (snakemake-read-rule 'targets))
   args))

;;;###autoload
(defun snakemake-build (&optional args)
  "Read and run a Snakemake command for building targets.

If a terminal is associated with the current Snakefile directory,
send the command there.  Otherwise, run the command with
`compile'.

To start a terminal for the current Snakefile directory, run
`\\[snakemake-term-start]'.

$ snakemake [ARGS] -- <targets>"
  (interactive (list (transient-args 'snakemake)))
  (let ((cmd (snakemake--make-command
              (or (snakemake-file-targets-at-point)
                  (snakemake-rule-at-point)
                  (list ""))
              args))
        (default-directory (snakemake-snakefile-directory)))
    (if (snakemake-term-use-p)
        (snakemake-term-send
         (read-from-minibuffer "Command to send to terminal: " cmd))
      (let ((compile-command cmd)
            (compilation-read-command t))
        (call-interactively #'compile)))))

(transient-define-argument snakemake:--allowed-rules ()
  :description "Allowed rules"
  :class 'transient-option
  :key "-a"
  :argument "--allowed-rules="
  :reader 'snakemake-read-rules)

(transient-define-argument snakemake:--cores ()
  :description "Number of cores"
  :class 'transient-option
  :key "-c"
  :argument "--cores=")

;;;###autoload (autoload 'snakemake "snakemake" nil t)
(transient-define-prefix snakemake
  "Transient for running Snakemake."
  :value '("--cores=all")
  ["Arguments"
   ("-C" "Use conda" "--use-conda")
   ("-f" "Force" "--force")
   ("-i" "Ignore temp()" "--notemp")
   ("-n" "Dry run" "--dryrun")
   ("-p" "Print shell commands" "-p")
   ("-r" "Print reason" "--reason")
   ("-t" "Touch files" "--touch")
   (snakemake:--allowed-rules)
   (snakemake:--cores)]
  ["Actions"
   [("c" "Edit and run command" snakemake-build)]
   [("p" "Build target at point" snakemake-build-targets-at-point)
    ("f" "Build file" snakemake-build-file-target)
    ("r" "Build rule" snakemake-build-rule-target)]])

;;;###autoload
(define-obsolete-function-alias 'snakemake-popup 'snakemake "2.0.0")

(provide 'snakemake)
;;; snakemake.el ends here
