;;; snakemake-mode.el --- Major mode for editing Snakemake files  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/snakemake-mode
;; Keywords: tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (magit-popup "2.4.0") (mmm-mode "0.5.4"))

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

;; Snakemake mode provides support for editing Snakemake [1] files.  It
;; builds on Python mode to provide fontification, indentation, and
;; imenu indexing for Snakemake's rule blocks.
;;
;; See also snakemake.el, which is packaged with snakemake-mode.el and
;; provides an interface for running Snakemake commands.
;;
;; If Snakemake mode is installed from MELPA, no additional setup is
;; required.  It will be loaded the first time a file named 'Snakefile'
;; is opened.
;;
;; Otherwise, put snakemake-mode.el in your `load-path' and add
;;
;;     (require 'snakemake-mode)
;;
;; to your initialization file.
;;
;; [1] https://bitbucket.org/snakemake/snakemake/wiki/browse/

;;; Code:

(require 'python)


;;; Customization

;;;###autoload
(defgroup snakemake-mode nil
  "Support for Snakemake files"
  :group 'tools)

(defcustom snakemake-mode-hook nil
  "Hook run when entering `snakemake-mode'."
  :type 'hook)

(defcustom snakemake-indent-field-offset 4
  "Offset for field indentation."
  :type 'integer)

(defcustom snakemake-indent-value-offset 4
  "Offset for field values that the line below the field key."
  :type 'integer)


;;; Regexp

(defconst snakemake-rule-or-subworkflow-re
  (rx (group symbol-start (or "rule" "subworkflow"))
      " "
      (group (one-or-more (or (syntax word) (syntax symbol))))
      ":")
  "Regexp matching a rule or subworkflow.")

(defconst snakemake-rule-or-subworkflow-line-re
  (concat "^" snakemake-rule-or-subworkflow-re)
  "Regexp matching a rule or subworkflow at start of line.")

(defconst snakemake-toplevel-command-re
  (rx line-start
      (group (or "configfile"
                 "include"
                 "onerror"
                 "onsuccess"
                 "ruleorder"
                 "workdir"))
      ":" (zero-or-more space))
  "Regexp matching other toplevel commands aside from 'rule'.")

(defconst snakemake-field-key-re
  (rx (group symbol-start
             (or "benchmark"
                 "input"
                 "log"
                 "message"
                 "output"
                 "params"
                 "priority"
                 "resources"
                 "run"
                 "script"
                 "shadow"
                 "shell"
                 "threads"
                 "version"
                 ;; Keys for subworkflow blocks
                 "snakefile"
                 "workdir"))
      ":")
  "Regexp matching a rule or subworkflow field key.")

(defconst snakemake-field-key-indented-re
  (concat  "^\\s-+" snakemake-field-key-re)
  "Regexp matching a field key, including indentation.")

(defconst snakemake-builtin-function-re
  (rx (group symbol-start
             (or "expand" "shell" "protected" "temp" "dynamic" "touch"))
      "(")
  "Regexp matching a call to a builtin Snakemake function.")


;;; Indentation

(defun snakemake-indent-line ()
  "Indent the current line.
Outside of rule blocks, handle indentation as it would be in a
Python mode buffer (using `python-indent-line-function').  Inside
rule blocks (or on a blank line directly below), call
`snakemake-indent-rule-line'."
  (interactive)
  (if (snakemake-in-rule-or-subworkflow-block-p)
      (snakemake-indent-rule-line)
    (python-indent-line-function)))

(defun snakemake-indent-rule-line ()
  "Indent line of a rule or subworkflow block.

Indent according the the first case below that is true.

- At the top of rule block

  Remove all indentation.

- At a rule field key ('input', 'output',...) or on the first
  field line of the block

  Indent the line to `snakemake-indent-field-offset'.

- On first line below a naked field key.

  Indent the line with `snakemake-indent-field-offset' plus
  `snakemake-indent-value-offset'.

- On any 'run' field value line except for the first value line.

  Indent according to Python mode.

- Before the current indentation

  Move point to the current indentation.

- Otherwise

  Cycle between indenting to `snakemake-indent-field-offset',
  indenting to the column of the previous field value.  If within
  a field value for a naked field key, add a step that indents
  according to Python mode."
  (let ((start-col (current-column))
        (start-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at-p (concat "^\\s-*" snakemake-rule-or-subworkflow-re))
        (delete-horizontal-space))
       ((or (looking-at-p (concat "^\\s-*" snakemake-field-key-re))
            (snakemake-first-field-line-p))
        (delete-horizontal-space)
        (indent-to snakemake-indent-field-offset))
       ((snakemake-below-naked-field-p)
        (delete-horizontal-space)
        (indent-to (+ snakemake-indent-field-offset
                      snakemake-indent-value-offset)))
       ((snakemake-run-field-line-p)
        (python-indent-line-function))
       ((>= start-col start-indent)
        (let ((prev-col (snakemake-previous-field-value-column)))
          (when prev-col
            (cond
             ((and (snakemake-naked-field-line-p)
                   (or (and (= start-indent 0)
                            (not (looking-at-p "^\\s-*$")))
                       (= start-indent prev-col)))
              (let (last-command)
                ;; ^ Don't let `python-indent-line' do clever things
                ;; when indent command is repeated.
                (python-indent-line-function))
              (when (= (current-column) start-indent)
                (delete-horizontal-space)
                (indent-to snakemake-indent-value-offset)))
             ((= start-indent snakemake-indent-field-offset)
              (delete-horizontal-space)
              (indent-to prev-col))
             (t
              (delete-horizontal-space)
              (indent-to snakemake-indent-field-offset))))))))
    (when (< (current-column) (current-indentation))
      (forward-to-indentation 0))))

(defun snakemake-first-field-line-p ()
  "Return non-nil if point is on first field line of block."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at-p snakemake-rule-or-subworkflow-re)))

(defun snakemake-in-rule-or-subworkflow-block-p ()
  "Return non-nil if point is in block or on first blank line following one."
  (let ((blank-p (lambda nil
                   (and (looking-at-p "^\\s-*$")
                        ;; Ignore newlines in docstrings.
                        (not (nth 3 (syntax-ppss)))))))
    (save-excursion
      (beginning-of-line)
      (when (funcall blank-p)
        (forward-line -1))
      (catch 'in-block
        (while (not (funcall blank-p))
          (cond ((looking-at-p snakemake-rule-or-subworkflow-re)
                 (throw 'in-block t))
                ((bobp)
                 (throw 'in-block nil)))
          (forward-line -1))))))

(defun snakemake-below-naked-field-p ()
  "Return non-nil if point is on first line below a naked field key."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at-p (concat snakemake-field-key-indented-re "\\s-*$"))))

(defun snakemake-naked-field-line-p ()
  "Return non-nil if point is on any line of naked field key.
This function assumes that point is in a rule or subworkflow
block (which includes being on a blank line immediately below a
block)."
  (save-excursion
    (let ((rule-start (save-excursion
                        (end-of-line)
                        (re-search-backward snakemake-rule-or-subworkflow-re
                                            nil t))))
      (end-of-line)
      (and (re-search-backward snakemake-field-key-indented-re
                               rule-start t)
           (goto-char (match-end 0))
           (looking-at-p "\\s-*$")))))

(defun snakemake-run-field-line-p ()
  "Return non-nil if point is on any line below a run field key.
This function assumes that point is in a rule or subworkflow
block (which includes being on a blank line immediately below a
block).  If it's not, it gives the wrong answer if below a rule
block whose last field is 'run'."
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
the first non-blank character after 'key:'.  Otherwise, it is the
column of the first non-blank character.

This function assumes that the previous line is a field value (in
other words, that point is at or beyond the third line of a rule
or subworkflow block."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    ;; Because of multiline fields, the previous line may not have a
    ;; key.
    (let ((rule-re (concat "\\(?:" snakemake-field-key-indented-re
                           "\\)*\\s-*\\S-")))
      (when (re-search-forward rule-re (point-at-eol) t)
        (1- (current-column))))))


;;; Imenu

(defun snakemake-imenu-create-index ()
  "Create Imenu index for rule blocks.
If `python-imenu-create-index' returns a non-nil value, also
include these results and append a \"(rule)\" to the index
label."
  (let ((py-index (python-imenu-create-index))
        (sm-index (snakemake--imenu-build-rule-index)))
    (if py-index
        (append (mapcar (lambda (x)
                          (cons (concat (car x) " (rule)") (cdr x)))
                        sm-index)
                py-index)
      sm-index)))

(defun snakemake--imenu-build-rule-index ()
  (goto-char (point-min))
  (let (index)
    (while (re-search-forward snakemake-rule-or-subworkflow-line-re nil t)
      (push (cons (match-string-no-properties 2)
                  (save-excursion (beginning-of-line)
                                  (point-marker)))
            index))
    (nreverse index)))


;;; Syntax-highlight embedded R code
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
 '((snakemake-R
    :submode R-mode
    :front ".*R\(\"\"\""
    :back ".*\"\"\"\)")))

(mmm-add-mode-ext-class 'snakemake-mode nil 'snakemake-R)


;;; Mode

(defvar snakemake-font-lock-keywords
  `((,snakemake-rule-or-subworkflow-line-re (1 font-lock-keyword-face)
                                            (2 font-lock-function-name-face))
    (,snakemake-toplevel-command-re 1 font-lock-keyword-face)
    (,snakemake-builtin-function-re 1 font-lock-builtin-face)
    (,snakemake-field-key-indented-re 1 font-lock-type-face)))

;;;###autoload
(define-derived-mode snakemake-mode python-mode "Snakemake"
  "Mode for editing Snakemake files."
  (set (make-local-variable 'imenu-create-index-function)
       #'snakemake-imenu-create-index)
  (set (make-local-variable 'indent-line-function) 'snakemake-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       `(,(append snakemake-font-lock-keywords python-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("Snakefile\\'" . snakemake-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.smrules\\'" . snakemake-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.snakefile\\'" . snakemake-mode))

(provide 'snakemake-mode)
;;; snakemake-mode.el ends here
