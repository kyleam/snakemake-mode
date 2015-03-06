;;; test-snakemake-mode.el --- Tests for snakemake-mode.el

;; Copyright (C) 2015 Kyle Meyer <kyle@kyleam.com>

;; Author:  Kyle Meyer <kyle@kyleam.com>

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

;;; Code:

(require 'snakemake-mode)
(require 'ert)

;; This is modified from `org-tests.el' (55c0708).
(defmacro snakemake-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Snakemake mode as the
active mode holding TEXT.  If the string \"<point>\" appears in
TEXT then remove it and place the point there before running
BODY, otherwise place the point at the beginning of the inserted
text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text))))
     (with-temp-buffer
       (snakemake-mode)
       (let ((point (string-match "<point>" inside-text)))
         (if point
             (progn
               (insert (replace-match "" nil nil inside-text))
               (goto-char (1+ (match-beginning 0))))
           (insert inside-text)
           (goto-char (point-min))))
       ,@body)))
(def-edebug-spec org-test-with-temp-text (form body))


(ert-deftest test-snakemake-mode/indentation ()
  "Test `snakemake-indent-line'."

  ;; At top of rule block
  (should
   (string=
    "rule abc:"
    (snakemake-with-temp-text
     "rule abc:"
     (snakemake-indent-line)
     (buffer-string))))

  ;; At top of rule block, repeated
  (should
   (string=
    "rule abc:"
    (snakemake-with-temp-text
     "rule abc:"
     (snakemake-indent-line)
     (snakemake-indent-line)
     (buffer-string))))

  ;; Below a rule block
  (should
   (string=
    "
rule abc:
    "
    (snakemake-with-temp-text
     "
rule abc:
<point>"
     (snakemake-indent-line)
     (buffer-string))))

  ;; Below a rule block, repeated
  (should
   (string=
    "
rule abc:
"
    (snakemake-with-temp-text
     "
rule abc:
<point>"
     (snakemake-indent-line)
     (snakemake-indent-line)
     (buffer-string))))

  ;; At rule field key.
  (should
   (string=
    "
rule abc:
    output:"
    (snakemake-with-temp-text
     "
rule abc:
<point>output:"
     (snakemake-indent-line)
     (buffer-string))))

  ;; At rule field key, repeated
  (should
   (string=
    "
rule abc:
    output:"
    (snakemake-with-temp-text
     "
rule abc:
<point>output:"
     (snakemake-indent-line)
     (snakemake-indent-line)
     (buffer-string))))

  ;; Below a filled rule field key
  (should
   (string=
    "
rule abc:
    output: 'file'
    "
    (snakemake-with-temp-text
     "
rule abc:
    output: 'file'
<point>"
     (snakemake-indent-line)
     (buffer-string))))

  ;; Below a filled rule field key, repeated once
  (should
   (string=
    "
rule abc:
    output: 'file'
            "
    (snakemake-with-temp-text
     "
rule abc:
    output: 'file'
<point>"
     (snakemake-indent-line)
     (snakemake-indent-line)
     (buffer-string))))

  ;; Below a filled rule field key, repeated twice
  (should
   (string=
    "
rule abc:
    output: 'file'
"
    (snakemake-with-temp-text
     "
rule abc:
    output: 'file'
<point>"
     (snakemake-indent-line)
     (snakemake-indent-line)
     (snakemake-indent-line)
     (buffer-string))))

  ;; Body of a run field
  (should
   (string=
    "
rule abc:
    run:
        with this:
            "
    (snakemake-with-temp-text
     "
rule abc:
    run:
        with this:
<point>"
     (snakemake-indent-line)
     (buffer-string))))

  ;; Outside a rule block
  (should
   (string=
    "
def ok():
    "
    (snakemake-with-temp-text
     "
def ok():
<point>"
     (snakemake-indent-line)
     (buffer-string)))))

(ert-deftest test-snakemake-mode/in-rule-block ()
  "Test `snakemake-in-rule-or-subworkflow-block-p'"

  ;; At top of block
  (snakemake-with-temp-text
   "
<point>rule abc:
    output: 'file'"
   (should (snakemake-in-rule-or-subworkflow-block-p)))

  ;; Body of block
  (snakemake-with-temp-text
   "
rule abc:
    output: <point>'file'"
   (should (snakemake-in-rule-or-subworkflow-block-p)))

  ;; First blank line after
  (snakemake-with-temp-text
   "
rule abc:
    output: 'file'
<point>"
   (should (snakemake-in-rule-or-subworkflow-block-p)))

  ;; Second blank line after
  (snakemake-with-temp-text
   "
rule abc:
    output: 'file'

<point>"
   (should-not (snakemake-in-rule-or-subworkflow-block-p)))

  ;; Before
  (snakemake-with-temp-text
   "<point>
rule abc:
    output: 'file'"
   (should-not (snakemake-in-rule-or-subworkflow-block-p)))

  ;; Subworkflow
  (snakemake-with-temp-text
   "
subworkflow otherworkflow:
<point>    workdir: '../path/to/otherworkflow'
    snakefile: '../path/to/otherworkflow/Snakefile'"
   (should (snakemake-in-rule-or-subworkflow-block-p))))

(ert-deftest test-snakemake-mode/run-field-line-p ()
  "Test `snakemake-run-field-line-p'."
  (snakemake-with-temp-text
   "
rule abc:
    run:
<point>"
   (should (snakemake-run-field-line-p)))
  (snakemake-with-temp-text
   "
rule abc:
    run:
        with file:
<point>"
   (should (snakemake-run-field-line-p)))
  (snakemake-with-temp-text
   "
rule abc:
    output: 'file'
<point>"
   (should-not (snakemake-run-field-line-p))))

(ert-deftest test-snakemake-mode/previous-field-value-column ()
  "Test `snakemake-previous-field-value-column'."
  (should (= 12
             (snakemake-with-temp-text
              "
rule abc:
    output: 'file'
<point>"
              (snakemake-previous-field-value-column))))
  (should (= 12
             (snakemake-with-temp-text
              "
rule abc:
    output: 'file',
            'another'
<point>"
              (snakemake-previous-field-value-column)))))


(provide 'test-snakemake-mode)
;;; test-snakemake-mode.el ends here
