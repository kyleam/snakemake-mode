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

(defcustom snakemake-indent-subrule-offset 4
  "Offset for subrule indentation."
  :type 'integer
  :group 'snakemake)

(defcustom snakemake-indent-run-offset 2
  "Additional offset for 'run' subrule value."
  :type 'integer
  :group 'snakemake)

(defcustom snakemake-compile-command-options nil
  "Flags to add to default Snakemake compilation command."
  :group 'snakemake
  :type '(repeat string))


;;; Regexp

(defconst snakemake-rule-re "\\(rule\\) \\([a-zA-Z0-9_]+\\):"
  "Regexp matching a rule.")

(defconst snakemake-rule-line-re (concat "^" snakemake-rule-re)
  "Regexp matching a rule at start of line.")

(defconst snakemake-toplevel-command-re "^\\(include\\|workdir\\):"
  "Regexp matching other toplevel commands aside from 'rule'.")

(defconst snakemake-subrule-re
  (concat "\\(input\\|output\\|shell\\|run\\|workdir\\|priority"
          "\\|message\\|threads\\|versions\\|resources\\|params\\):")
  "Regexp matching a subrule key.")

(defconst snakemake-subrule-indented-re (concat  "^[ \t]+" snakemake-subrule-re)
  "Regexp matching a subrule key, including indentation.")

(defconst snakemake-builtin-function-re
  "\\(expand\\|shell\\|protected\\|temp\\|dynamic\\)("
  "Regexp matching a builtin functions.")


;;; Indentation

(defun snakemake-indent-line ()
  "Indent the current line.

Outside of rule blocks, indentation is handled as it would be in
a Python mode buffer (using `python-indent-line-function').

Inside rule blocks (or on a blank line directly below),
indentation is determined by the location within the rule block.

- At the top of rule block

  All indentation is removed.

- At a rule subkey ('input', 'output',...)

  The line is indented to `snakemake-indent-subrule-offset'.

- Below a 'run' subkey

  The first line below 'run' will be indented to
  `snakemake-indent-subrule-offset' plus
  `snakemake-indent-run-offset'. Other lines are indented with
  `python-indent-line-function'.

- Otherwise

  Alternate between no indentation,
  `snakemake-indent-subrule-offset', and the column of the
  previous subrule value."
  (interactive)
  (if (snakemake-in-rule-block-p)
      (snakemake-indent-rule-line)
    (python-indent-line-function)))

(defun snakemake-indent-rule-line ()
  (save-excursion
    (let ((start-indent (current-indentation)))
      (beginning-of-line)
      (cond
       ((looking-at (concat "^[ \t]*" snakemake-rule-re))
        (delete-horizontal-space))
       ((looking-at (concat "^[ \t]*" snakemake-subrule-re))
        (delete-horizontal-space)
        (indent-to snakemake-indent-subrule-offset))
       ((snakemake-run-subrule-first-line-p)
        (indent-to (+ snakemake-indent-subrule-offset
                      snakemake-indent-run-offset)))
       ((snakemake-run-subrule-line-p)
        (python-indent-line-function))
       ((< start-indent snakemake-indent-subrule-offset)
        (delete-horizontal-space)
        (indent-to snakemake-indent-subrule-offset))
       (t
        (let ((prev-col (snakemake-previous-subrule-value-column)))
          (when prev-col
            (delete-horizontal-space)
            (when (< start-indent prev-col)
              (indent-to prev-col))))))))
  (if (< (current-column) (current-indentation))
      (forward-to-indentation 0)))

(defun snakemake-in-rule-block-p ()
  "Point is in or on blank line following a rule block."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^ *$")
      (forward-line -1))
    (end-of-line)
    (let ((start (point)))
      (and (re-search-backward snakemake-rule-re nil t)
           (not (re-search-forward "^ *$" start t))))))

(defun snakemake-run-subrule-first-line-p ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (when (re-search-forward "^[ \t]+run:" (point-at-eol) t)
      t)))

(defun snakemake-run-subrule-line-p ()
  (save-excursion
    (let ((rule-start (save-excursion
                        (end-of-line)
                        (re-search-backward snakemake-rule-re nil t))))
      (forward-line -1)
      (end-of-line)
      (re-search-backward snakemake-subrule-indented-re rule-start t)
      (string= (match-string 1) "run"))))

(defun snakemake-previous-subrule-value-column ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    ;; Because of multiline subrules, the previous line may not have a
    ;; key.
    (let ((rule-re (concat "\\(?:" snakemake-subrule-indented-re
                           "\\)* *[^ ]")))
      (if (re-search-forward rule-re (point-at-eol) t)
          (1- (current-column))))))


;;; Mode

(defvar snakemake-font-lock-keywords
  `((,snakemake-rule-line-re (1 font-lock-keyword-face)
                             (2 font-lock-function-name-face))
    (,snakemake-toplevel-command-re 1 font-lock-keyword-face)
    (,snakemake-builtin-function-re 1 font-lock-builtin-face)
    (,snakemake-subrule-indented-re 1 font-lock-type-face)))

(defun snakemake-set-imenu-generic-expression ()
  ;; Disable `python-info-current-defun'.
  (setq imenu-extract-index-name-function nil)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression `((nil ,snakemake-rule-line-re 2))))

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
