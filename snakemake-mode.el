;;; snakemake-mode.el --- Major mode for editing Snakemake files

;; Copyright (C) 2014 Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/snakemake-mode
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Snakemake mode provides support for editing Snakemake [1] files. It
;; builds on Python mode to provide fontification, indentation, and
;; imenu indexing for Snakemake's rule blocks.
;;
;; If Snakemake mode is installed from MELPA, no additional setup is
;; required. It will be loaded the first time a file named 'Snakefile'
;; is opened.
;;
;; Otherwise, put snakemake-mode.el in your `load-path' and add
;;
;;     (require 'snakemake-mode)
;;
;; to your initialization file.
;;
;; [1] https://bitbucket.org/johanneskoester/snakemake/wiki/browse/

;;; Code:

(require 'python)


;;; Customization

;;;###autoload
(defgroup snakemake nil
  "Support for Snakemake files"
  :group 'tools
  :prefix "snakemake-")

(defcustom snakemake-mode-hook nil
  "Hook run when entering `snakemake-mode'."
  :type 'hook
  :group 'snakemake)

(defcustom snakemake-indent-field-offset 4
  "Offset for field indentation."
  :type 'integer
  :group 'snakemake)

(defcustom snakemake-indent-run-offset 2
  "Additional offset for 'run' field value."
  :type 'integer
  :group 'snakemake)

(defcustom snakemake-compile-command-options nil
  "Flags to add to default Snakemake compilation command."
  :group 'snakemake
  :type '(repeat string))


;;; Regexp

(defconst snakemake-rule-or-subworkflow-re
  "\\(rule\\|subworkflow\\) \\([a-zA-Z0-9_]+\\):"
  "Regexp matching a rule or subworkflow.")

(defconst snakemake-rule-or-subworkflow-line-re
  (concat "^" snakemake-rule-or-subworkflow-re)
  "Regexp matching a rule or subworkflow at start of line.")

(defconst snakemake-toplevel-command-re "^\\(include\\|workdir\\):"
  "Regexp matching other toplevel commands aside from 'rule'.")

(defconst snakemake-field-key-re
  (concat "\\(input\\|output\\|shell\\|run\\|workdir\\|priority"
          "\\|message\\|threads\\|versions\\|resources\\|params"
          "\\|snakefile\\):")
  "Regexp matching a rule or subworkflow field key.")

(defconst snakemake-field-key-indented-re
  (concat  "^[ \t]+" snakemake-field-key-re)
  "Regexp matching a field key, including indentation.")

(defconst snakemake-builtin-function-re
  "\\(expand\\|shell\\|protected\\|temp\\|dynamic\\)("
  "Regexp matching a builtin functions.")


;;; Indentation

(defun snakemake-indent-line ()
  "Indent the current line.
Outside of rule blocks, indentation is handled as it would be in
a Python mode buffer (using `python-indent-line-function').
Inside rule blocks (or on a blank line directly below),
`snakemake-indent-rule-line' is called."
  (interactive)
  (if (snakemake-in-rule-or-subworkflow-block-p)
      (snakemake-indent-rule-line)
    (python-indent-line-function)))

(defun snakemake-indent-rule-line ()
  "Indent rule line.

- At the top of rule block

  All indentation is removed.

- At a rule field key ('input', 'output',...)

  The line is indented to `snakemake-indent-field-offset'.

- Below a 'run' subkey

  The first line below 'run' will be indented to
  `snakemake-indent-field-offset' plus
  `snakemake-indent-run-offset'. Other lines are indented with
  `python-indent-line-function'.

- Otherwise

  Alternate between no indentation,
  `snakemake-indent-field-offset', and the column of the previous
  field value."
  (save-excursion
    (let ((start-indent (current-indentation)))
      (beginning-of-line)
      (cond
       ((looking-at (concat "^[ \t]*" snakemake-rule-or-subworkflow-re))
        (delete-horizontal-space))
       ((looking-at (concat "^[ \t]*" snakemake-field-key-re))
        (delete-horizontal-space)
        (indent-to snakemake-indent-field-offset))
       ((snakemake-run-field-first-line-p)
        (delete-horizontal-space)
        (indent-to (+ snakemake-indent-field-offset
                      snakemake-indent-run-offset)))
       ((snakemake-run-field-line-p)
        (python-indent-line-function))
       ((< start-indent snakemake-indent-field-offset)
        (delete-horizontal-space)
        (indent-to snakemake-indent-field-offset))
       (t
        (let ((prev-col (snakemake-previous-field-value-column)))
          (when prev-col
            (delete-horizontal-space)
            (when (< start-indent prev-col)
              (indent-to prev-col))))))))
  (if (< (current-column) (current-indentation))
      (forward-to-indentation 0)))

(defun snakemake-in-rule-or-subworkflow-block-p ()
  "Point is in block or on first blank line following one."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^ *$")
      (forward-line -1))
    (end-of-line)
    (let ((start (point)))
      (and (re-search-backward snakemake-rule-or-subworkflow-re nil t)
           (not (re-search-forward "^ *$" start t))))))

(defun snakemake-run-field-first-line-p ()
  "Point is on the first line below a run field key."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (when (re-search-forward "^[ \t]+run:" (point-at-eol) t)
      t)))

(defun snakemake-run-field-line-p ()
  "Point is on any line below a run field key.
This function assumes that
`snakemake-in-rule-or-subworkflow-block-p' is true. If it's not,
it will give the wrong answer if below a rule block whose last
field is 'run'."
  (save-excursion
    (let ((rule-start (save-excursion
                        (end-of-line)
                        (re-search-backward snakemake-rule-or-subworkflow-re
                                            nil t))))
      (forward-line -1)
      (end-of-line)
      (re-search-backward snakemake-field-key-indented-re rule-start t)
      (string= (match-string 1) "run"))))

(defun snakemake-previous-field-value-column ()
  "Get column for previous field value.
If directly below a field key, this corresponds to the column for
the first non-blank character after 'key:'. Otherwise, it is the
column of the first non-blank character."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    ;; Because of multiline fields, the previous line may not have a
    ;; key.
    (let ((rule-re (concat "\\(?:" snakemake-field-key-indented-re
                           "\\)* *[^ ]")))
      (if (re-search-forward rule-re (point-at-eol) t)
          (1- (current-column))))))


;;; Mode

(defvar snakemake-font-lock-keywords
  `((,snakemake-rule-or-subworkflow-line-re (1 font-lock-keyword-face)
                             (2 font-lock-function-name-face))
    (,snakemake-toplevel-command-re 1 font-lock-keyword-face)
    (,snakemake-builtin-function-re 1 font-lock-builtin-face)
    (,snakemake-field-key-indented-re 1 font-lock-type-face)))

(defun snakemake-set-imenu-generic-expression ()
  "Extract rule names for `imenu' index."
  ;; Disable `python-info-current-defun'
  (setq imenu-extract-index-name-function nil)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression
        `((nil ,snakemake-rule-or-subworkflow-line-re 2))))

(add-hook 'snakemake-mode-hook 'snakemake-set-imenu-generic-expression)

;;;###autoload
(define-derived-mode snakemake-mode python-mode "Snakemake"
  "Mode for editing Snakemake files."
  (set (make-local-variable 'indent-line-function) 'snakemake-indent-line)
  (font-lock-add-keywords nil snakemake-font-lock-keywords)
  (set (make-local-variable 'compile-command)
       (mapconcat 'identity
                  (cons "snakemake" snakemake-compile-command-options) " ")))

;;;###autoload
(add-to-list 'auto-mode-alist '("Snakefile\\'" . snakemake-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.smrules\\'" . snakemake-mode))

(provide 'snakemake-mode)

;;; snakemake-mode.el ends here
