;;; snakemake-test.el --- Test snakemake{,-mode}.el

;; Copyright (C) 2015-2018 Kyle Meyer

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'snakemake-mode)
(require 'snakemake)
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
(def-edebug-spec snakemake-with-temp-text (form body))

(defmacro snakemake-with-temp-dir (&rest body)
  "Run BODY in a temporary directory with Snakefile.
`snakemake-test-dir' is bound to top-level directory."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
     (let* ((snakemake-test-dir (file-name-as-directory
                                 (make-temp-file "sm-test-dir" t)))
            (snakemake-root-dir-function `(lambda () ,snakemake-test-dir)))
       (unwind-protect
           (let ((default-directory snakemake-test-dir))
             (mkdir "subdir")
             (write-region "\

rule aa:
    output: \"aa.out\"
    shell: \"echo aa.content > {output}\"

rule bb:
    input: \"aa.out\"
    output: \"bb.out\"
    shell: \"cat {input} > {output}\"

rule cc_wildcards:
    input: \"bb.out\"
    output: \"{name}.outwc\"
    shell: \"cat {input} > {output}\"

rule dd_subdir:
    input: \"aa.out\"
    output: \"subdir/dd.out\"
    shell: \"cat {input} > {output}\"

rule:
    input: \"anon.in\"
    output: \"anon.out\"
    shell: \"cat {input} > {output}\""
                           nil
                           "Snakefile")
             ,@body)
         (delete-directory snakemake-test-dir t)))))
(def-edebug-spec snakemake-with-temp-dir (body))


;;; snakemake-mode.el

;;;; Indentation

(ert-deftest snakemake-test-indent-line/at-rule-block ()
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
  (should
   (string=
    "rule abc :"
    (snakemake-with-temp-text
        "     rule abc :"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "rule abc:  "
    (snakemake-with-temp-text
        "    rule abc:  <point>"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
if True:
    rule abc:"
    (snakemake-with-temp-text
        "
if True:
rule abc:<point>"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
if True:
rule abc:"
    (snakemake-with-temp-text
        "
if True:
    <point>rule abc:"
      (snakemake-indent-line 'prev)
      (buffer-string)))))

(ert-deftest snakemake-test-indent-line/outside-rule ()
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

(ert-deftest snakemake-test-indent-line/toplevel-command ()
  (should
   (string=
    "include: \"somefile\""
    (snakemake-with-temp-text
        "include: \"somefile\""
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "include: \"somefile\""
    (snakemake-with-temp-text
        "   include: \"somefile\""
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
if True:
    include: \"somefile\"
"
    (snakemake-with-temp-text
        "
if True:
<point>include: \"somefile\"
"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
include:
    \"somefile\"
"
    (snakemake-with-temp-text
        "
include:
<point>\"somefile\"
"
      (snakemake-indent-line)
      (buffer-string)))))

(ert-deftest snakemake-test-indent-line/field-key ()
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
      (snakemake-indent-line 'prev)
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
      (snakemake-indent-line 'prev)
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
    output :"
    (snakemake-with-temp-text
        "
rule abc:
    input: 'infile'
<point>output :"
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
      (snakemake-indent-line 'prev)
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

(ert-deftest snakemake-test-indent-line/field-value ()
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
      (snakemake-indent-line 'prev)
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
  (should
   (string=
    "
rule abc:
    output:  # comment
        "
    (snakemake-with-temp-text
        "
rule abc:
    output:  # comment
              <point>"
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
      (snakemake-indent-line 'prev)
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
<point>            'text'"
      (snakemake-indent-line 'prev)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:
        'file'
        'text'"
    (snakemake-with-temp-text
        "
rule abc:
    output:
        'file'
<point>            'text'"
      (snakemake-indent-line)
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    output:  # comment
        'file'
        'text'"
    (snakemake-with-temp-text
        "
rule abc:
    output:  # comment
        'file'
<point>            'text'"
      (snakemake-indent-line)
      (buffer-string))))
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

(ert-deftest snakemake-test/indent-region ()
  (should
   (string=
    "
rule abc:
    input: 'infile'
    output:"
    (snakemake-with-temp-text
        "
<point>rule abc:
input: 'infile'
output:"
      (indent-region (point) (point-max))
      (buffer-string))))
  (should
   (string=
    "
if True:
    rule abc:
        input: 'infile'
        output:"
    (snakemake-with-temp-text
        "
if True:
<point>rule abc:
input: 'infile'
output:"
      (indent-region (point) (point-max))
      (buffer-string))))
  (should
   (string=
    "
rule abc:
    input:
        one='one', two='two'
    output: 'out'
    run:
        with open(input.one) as ifh:
            with open(output.out, 'w') as ofh:
                ofh.write(ifh.read())"
    (snakemake-with-temp-text
        "
<point>rule abc:
input:
one='one', two='two'
output: 'out'
run:
with open(input.one) as ifh:
with open(output.out, 'w') as ofh:
ofh.write(ifh.read())"
      (indent-region (point) (point-max))
      (buffer-string))))
  (should
   (string=
    "
x = [1,
     2,
     3,]"
    (snakemake-with-temp-text
     "
<point>x = [1,
2,
3,]"
     (indent-region (point) (point-max))
     (buffer-string)))))

;;;; Info and navigation

(ert-deftest snakemake-test-block-info ()
  (should-not
   (snakemake-with-temp-text
       "
rule abc:
    output: 'file'"
     (snakemake-block-info))))

(ert-deftest snakemake-test-beginning-of-block ()
  (should
   (string= "
rule abc:
    output: 'file'"
            (snakemake-with-temp-text
                "
<point>rule abc:
    output: 'file'"
              (snakemake-beginning-of-block)
              (buffer-substring (point) (point-max)))))
  (should
   (string= "rule abc:
    output: 'file'"
            (snakemake-with-temp-text
                "
rule abc:
    output: <point>'file'"
              (snakemake-beginning-of-block)
              (buffer-substring (point) (point-max)))))
  (should
   (string= "rule abc:
    output: 'file'

"
            (snakemake-with-temp-text
                "
rule abc:
    output: 'file'

<point>"
              (snakemake-beginning-of-block)
              (buffer-substring (point) (point-max)))))
  (should
   (string= "rule abc:
     \"\"\"docstring header

     docstring line
     \"\"\"
    output: 'file'"
            (snakemake-with-temp-text
                "
rule abc:
     \"\"\"docstring header

     docstring line
     \"\"\"
    output: 'file'<point>"
              (snakemake-beginning-of-block)
              (buffer-substring (point) (point-max)))))
  (should
   (string= "subworkflow otherworkflow:
    workdir: '../path/to/otherworkflow'
    snakefile: '../path/to/otherworkflow/Snakefile'"
            (snakemake-with-temp-text
                "
subworkflow otherworkflow:
<point>    workdir: '../path/to/otherworkflow'
    snakefile: '../path/to/otherworkflow/Snakefile'"
              (snakemake-beginning-of-block)
              (buffer-substring (point) (point-max))))))

(ert-deftest snakemake-test-end-of-block ()
  (should
   (string= "
rule abc:
    output: 'file'


"
            (snakemake-with-temp-text
                "
rule abc:
<point>    output: 'file'


"
              (snakemake-end-of-block)
              (buffer-substring (point-min) (point)))))
  (should
   (string= "
rule abc:
    output: 'file'
"
            (snakemake-with-temp-text
                "
rule abc:<point>
    output: 'file'

rule xyz:
    input: 'file'"
              (snakemake-end-of-block)
              (buffer-substring (point-min) (point)))))
  (should
   (string= "
rule abc:
    output: 'file'"
            (snakemake-with-temp-text
                "
rule abc:<point>
    output: 'file'
rule xyz:
    input: 'file'"
              (snakemake-end-of-block)
              (buffer-substring (point-min) (point))))))

(ert-deftest snakemake-test-block-or-defun-name ()
  (should
   (string= "abc"
            (snakemake-with-temp-text
                "
rule abc:
<point>    output: 'file'
"
              (snakemake-block-or-defun-name))))
  (should
   (string= "xyz"
            (snakemake-with-temp-text
                "
rule abc:
    output: 'file'

<point>def xyz():
    pass
"
              (snakemake-block-or-defun-name))))
  (should-not
   (snakemake-with-temp-text
       "
rule abc:
    output: 'file'
"
     (snakemake-block-or-defun-name))))


;;; snakemake.el

(ert-deftest snakemake-test-snakefile-directory ()
  (snakemake-with-temp-dir
    (should (equal default-directory (snakemake-snakefile-directory)))
    (let ((topdir default-directory))
      (should (equal topdir
                     (let ((default-directory "subdir"))
                       (snakemake-snakefile-directory)))))))

(ert-deftest snakemake-test-split-lines ()
  (should
   (equal '("abc" "123")
          (snakemake--split-lines "\
abc
123
two words")))
  (should
   (equal '("abc" "123")
          (snakemake--split-lines "\
abc

123

two words")))
  (should
   (equal '("abc")
          (snakemake--split-lines "\
abc
123
two words"
                                  'remove-num))))

(ert-deftest snakemake-test-rule-targets ()
  (should
   (equal '("aa" "bb" "dd_subdir")
          (snakemake-with-temp-dir
            (snakemake-rule-targets)))))

(ert-deftest snakemake-test-all-rules ()
  (should
   (equal '("aa" "bb" "cc_wildcards" "dd_subdir")
          (snakemake-with-temp-dir
            (snakemake-all-rules)))))

(ert-deftest snakemake-test-file-targets ()
  (should
   (equal
    (and snakemake-file-target-program
         '("aa.out" "bb.out" "subdir/dd.out" "anon.out"))
    (snakemake-with-temp-dir
      (snakemake-file-targets)))))

(ert-deftest snakemake-test-check-target ()
  (should
   (snakemake-with-temp-dir
     (snakemake-check-target "aa.out")))
  (should-not
   (snakemake-with-temp-dir
     (snakemake-check-target "aa.out.not-target")))
  (should
   (snakemake-with-temp-dir
     (snakemake-check-target "aa")))
  (should-not
   (snakemake-with-temp-dir
     (snakemake-check-target "cc_wildcards")))
  ;; Errors with the Snakefile, like ambiguous rules and syntax
  ;; errors, should be reported as errors rather than treated as
  ;; invalid targets.
  (should-error
   (snakemake-with-temp-dir
     (write-region "\ndef incomplete_def:"
                   nil
                   "Snakefile"
                   'append)
     (snakemake-check-target "aa"))
   :type 'snakemake-error)
  (should-error
   (snakemake-with-temp-dir
     (write-region "\

rule aa:
    output: \"aa.ambig.out\"
    shell: \"echo aa.ambig.content > {output}\""
                   nil
                   "Snakefile"
                   'append)
     (snakemake-check-target "aa"))
   :type 'snakemake-error)
  ;; Write-protected targets should be recognized as valid targets
  ;; despite Snakemake throwing an error.
  (should
   (snakemake-with-temp-dir
     (write-region "" nil "bb.out")
     (set-file-modes "bb.out" (file-modes-symbolic-to-number "u=r"))
     (write-region "" nil "aa.out")
     (snakemake-check-target "bb.out"))))

(ert-deftest snakemake-test-org-link-file-targets ()
  (should (equal '("/path/to/fname")
                 (with-temp-buffer
                   (org-mode)
                   (insert "\n[[file:/path/to/fname][descr]]\n")
                   (forward-line -1)
                   (snakemake-org-link-file-targets)))))

(ert-deftest snakemake-test-region-file-targets ()
  (let ((files '("/path/to/fname" "fname2" "CAP")))
    (should (equal (mapcar #'expand-file-name files)
                   (with-temp-buffer
                     (insert (mapconcat #'identity files "\n"))
                     (snakemake-region-file-targets
                      (point-min) (point-max)))))
    (should (equal (mapcar #'expand-file-name files)
                   (with-temp-buffer
                     (insert (mapconcat #'identity files ","))
                     (snakemake-region-file-targets
                      (point-min) (point-max)))))
    (should (equal (mapcar #'expand-file-name files)
                   (with-temp-buffer
                     (insert (car files))
                     (insert ?\n)
                     (insert (mapconcat #'identity (cdr files) " "))
                     (snakemake-region-file-targets
                      (point-min) (point-max)))))))

(ert-deftest snakemake-test-file-targets-at-point ()
  (should
   (equal '("aa.out")
          (snakemake-with-temp-dir
            (with-temp-buffer
              (insert "aa.out")
              (beginning-of-line)
              (snakemake-file-targets-at-point 'check)))))
  (should-not
   (snakemake-with-temp-dir
     (with-temp-buffer
       (insert "aa.out.not-target")
       (beginning-of-line)
       (snakemake-file-targets-at-point 'check))))
  (should
   (equal '("aa.out.not-target")
          (snakemake-with-temp-dir
            (with-temp-buffer
              (insert "aa.out.not-target")
              (beginning-of-line)
              (snakemake-file-targets-at-point))))))

(ert-deftest snakemake-test-rule-at-point ()
  (should
   (equal '("aa")
          (snakemake-with-temp-dir
            (with-temp-buffer
              (snakemake-mode)
              (insert-file-contents "Snakefile")
              (re-search-forward "rule aa:")
              (snakemake-rule-at-point 'target)))))
  (should
   (equal '("cc_wildcards")
          (snakemake-with-temp-dir
            (with-temp-buffer
              (snakemake-mode)
              (insert-file-contents "Snakefile")
              (re-search-forward "rule cc_wildcards:")
              (snakemake-rule-at-point)))))
  (should-not
   (snakemake-with-temp-dir
    (with-temp-buffer
      (snakemake-mode)
      (insert-file-contents "Snakefile")
      (re-search-forward "rule cc_wildcards:")
      (snakemake-rule-at-point 'target))))
  (should-not
   (snakemake-with-temp-dir
     (with-temp-buffer
       (snakemake-mode)
       (insert-file-contents "Snakefile")
       (re-search-forward "rule:")
       (snakemake-rule-at-point)))))

(provide 'snakemake-test)
;;; snakemake-test.el ends here
