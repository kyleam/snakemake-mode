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

(require 'cl-lib)
(require 'snakemake-mode)
(require 'ert)

;; This is modified from `org-tests.el' (55c0708).
(defmacro snakemake-with-temp-text (text &rest body)
  "Run body in a temporary Snakemake mode buffer.

Fill the buffer with TEXT.  If the string \"<point>\" appears in
TEXT then remove it and place the point there before running
BODY, otherwise place the point at the beginning of the inserted
text.

Also, mute messages."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
     (let ((inside-text (if (stringp ,text) ,text (eval ,text))))
       (with-temp-buffer
         (snakemake-mode)
         (let ((point (string-match "<point>" inside-text)))
           (if point
               (progn
                 (insert (replace-match "" nil nil inside-text))
                 (goto-char (1+ (match-beginning 0))))
             (insert inside-text)
             (goto-char (point-min))))
         ,@body))))
(def-edebug-spec org-test-with-temp-text (form body))



;;; Indentation

(ert-deftest test-snakemake-mode/indentation-at-rule-block ()
  "Test `snakemake-indent-line' at top of rule block."

  ;; Always shift first line of block to column 0.
  (should
   (string=
    "rule abc:"
    (snakemake-with-temp-text
        "rule abc:"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "rule abc:"
    (snakemake-with-temp-text
        "     rule abc:"
      (snakemake-indent-line)
      (buffer-string))))

  ;; Don't move point if beyond column 0.
  (should
   (string=
    "rule abc:  "
    (snakemake-with-temp-text
        "    rule abc:  <point>"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "rule "
    (snakemake-with-temp-text
        "    rule <point>abc:  <point>"
      (snakemake-indent-line)
      (buffer-substring (point-min) (point))))))

(ert-deftest test-snakemake-mode/indentation-outside-rule ()
  "Test `snakemake-indent-line' outside rule block."
  ;; Use standard Python mode indentation outside of rule blocks.
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

(ert-deftest test-snakemake-mode/indentation-field-key ()
  "Test `snakemake-indent-line' on field key line."

  ;; Always indent first line to `snakemake-indent-field-offset'.
  ;; Move point to `snakemake-indent-field-offset' if it is before any
  ;; text on the line.
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
  (should
   (string=
    "
rule abc:
    text"
    (snakemake-with-temp-text
        "
rule abc:
text<point>"
      (snakemake-indent-line)
      (buffer-substring (point-min) (point)))))
  (should
   (string=
    "
rule abc:
    te"
    (snakemake-with-temp-text
        "
rule abc:
te<point>xt"
      (snakemake-indent-line)
      (buffer-substring (point-min) (point)))))

  ;; Always indent field key to `snakemake-indent-field-offset'.
  ;; Move point to `snakemake-indent-field-offset' if it is before any
  ;; text on the line.
  (should
   (string=
    "
rule abc:
    input: 'infile'
    output:"
    (snakemake-with-temp-text
        "
rule abc:
    input: 'infile'
<point>output:"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    input: 'infile'
    output:"
    (snakemake-with-temp-text
        "
rule abc:
    input: 'infile'
<point>output:"
      (snakemake-indent-line)
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    input: 'infile'
    output:  "
    (snakemake-with-temp-text
        "
rule abc:
    input: 'infile'
output:  <point>"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    input: 'infile'
    "
    (snakemake-with-temp-text
        "
rule abc:
    input: 'infile'
<point>  output:"
      (snakemake-indent-line)
      (buffer-substring (point-min) (point))))))

(ert-deftest test-snakemake-mode/indentation-field-value ()
  "Test `snakemake-indent-line' on field value line."

  ;; Always indent line below naked field key to
  ;; `snakemake-indent-field-offset' +
  ;; `snakemake-indent-value-offset'.  Move point to to this position
  ;; as well if it is before any text on the line.
  (should
   (string=
    "
rule abc:
    output:
        "
    (snakemake-with-temp-text
        "
rule abc:
    output:
<point>"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:
        "
    (snakemake-with-temp-text
        "
rule abc:
    output:
<point>"
      (snakemake-indent-line)
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:
        "
    (snakemake-with-temp-text
        "
rule abc:
    output:
              <point>"
      (snakemake-indent-line)
      (buffer-string))))

  ;; Add step with Python indentation for non-blank lines under naked
  ;; field keys.  Field keys with values starting on the same line do
  ;; not use Python indentation because this is invalid syntax in
  ;; Snakemake.
  (should
   (string=
    "
rule abc:
    output: 'file{}{}'.format('one',
    'two'"
    (snakemake-with-temp-text
        "
rule abc:
    output: 'file{}{}'.format('one',
<point>'two'"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:
        'file{}{}'.format('one',
                          'two'"
    (snakemake-with-temp-text
        "
rule abc:
    output:
        'file{}{}'.format('one',
<point>'two'"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:
        'file{}{}'.format('one',
    "
    (snakemake-with-temp-text
        "
rule abc:
    output:
        'file{}{}'.format('one',
<point>"
      (snakemake-indent-line)
      (buffer-string))))

  ;; On non-naked field key cycle indentation between
  ;; `snakemake-indent-field-offset' and column of previous field
  ;; value.  If point is before any text on the line, move it to the
  ;; start of the text instead.
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
  (should
   (string=
    "
rule abc:
    output: 'file'
    'text'"
    (snakemake-with-temp-text
        "
rule abc:
    output: 'file'
<point>'text'"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output: 'file'
            'text'"
    (snakemake-with-temp-text
        "
rule abc:
    output: 'file'
<point>'text'"
      (snakemake-indent-line)
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output: 'file'
    'text' "
    (snakemake-with-temp-text
        "
rule abc:
    output: 'file'
'text' <point>"
      (snakemake-indent-line)
      (buffer-string))))
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
<point>  'text'"
      (snakemake-indent-line)
      (buffer-substring (point-min) (point)))))
  (should
   (string=
    "
rule abc:
    output: 'file'
    'text'"
    (snakemake-with-temp-text
        "
rule abc:
    output: 'file'
<point>  'text'"
      (snakemake-indent-line)
      (snakemake-indent-line)
      (buffer-string))))

  ;; Indent body of run field according to Python mode.
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
      (buffer-string)))))


;;; Other

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


  ;; Blank line in docstring
  (snakemake-with-temp-text
      "
rule abc:
     \"\"\"docstring header

     docstring line
     \"\"\"
    output: 'file'<point>"
    (should (snakemake-in-rule-or-subworkflow-block-p)))

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

(ert-deftest test-snakemake-mode/first-field-line-p ()
  "Test `snakemake-first-field-line-p'."
  (snakemake-with-temp-text
      "
rule abc:
<point>"
    (should (snakemake-first-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
<point>    output: 'file'"
    (should (snakemake-first-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output:
<point>"
    (should-not (snakemake-first-field-line-p))))

(ert-deftest test-snakemake-mode/below-naked-field-p ()
  "Test `snakemake-below-naked-field-p'."
  (snakemake-with-temp-text
      "
rule abc:
    output:
<point>"
    (should (snakemake-below-naked-field-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output: 'file'
<point>"
    (should-not (snakemake-below-naked-field-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output: <point>"
    (should-not (snakemake-below-naked-field-p))))

(ert-deftest test-snakemake-mode/naked-field-line-p ()
  "Test `snakemake-naked-field-line-p'."
  (snakemake-with-temp-text
      "
rule abc:
    output:
<point>"
    (should (snakemake-naked-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output:
        'file',
         <point>"
    (should (snakemake-naked-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output: <point>"
    (should (snakemake-naked-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
    output: 'file'
<point>"
    (should-not (snakemake-naked-field-line-p)))
  (snakemake-with-temp-text
      "
rule abc:
    input:
        'infile'
    output: 'file'
<point>"
    (should-not (snakemake-naked-field-line-p))))

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
