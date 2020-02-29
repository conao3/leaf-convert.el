;;; leaf-convert-test.el --- Test definitions for leaf-convert  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf-convert.el

;; Copyright (C) 2020  Naoya Yamashita

;; This program is free software: you can redistribute it and/or modify
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

;; Test definitions for `leaf-convert'.


;;; Code:

(require 'cort-test)
(require 'leaf-convert)

(defmacro cort-deftest-with-equal (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf-fn)
         (uiop uiop-fn))))
   => (cort-deftest leaf/disabled
        '((:equal 'asdf asdf-fn)
          (:equal 'uiop uiop-fn)))"
  (declare (indent 1))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal ,(cadr elm) ,(car elm)))
               (cadr form))))

(defmacro cort-deftest-with-macroexpand (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf)
         (uiop uiop))))
   => (cort-deftest leaf/disabled
        '((:equal 'asdf
                  (macroexpand-1 'asdf))
          (:equal 'uiop
                  (macroexpand-1 'uiop))))"
  (declare (indent 1))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal
                   ',(cadr elm)
                   (macroexpand-1 ',(car elm))))
               (cadr form))))


;;; test definitions

(cort-deftest-with-equal leaf-convert/convert-contents
  '(((leaf-convert-contents
      (leaf-convert-contents-new))
     '(leaf leaf-convert))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :name "some-package"))
     '(leaf some-package))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :name 'some-package))
     '(leaf some-package))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :disabled t))
     '(leaf leaf-convert
        :disabled t))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :disabled :leaf-convert--nil))
     '(leaf leaf-convert
        :disabled nil))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :config '((leaf-keywords-init))))
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)))

    ((leaf-convert-contents
      (leaf-convert-contents-new
       :config '((leaf-keywords-init)
                 (leaf-keywords-teardown))))
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)
        (leaf-keywords-teardown)))))

(cort-deftest-with-equal leaf-convert/progn
  '(((leaf-convert
      (progn
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))))
     '(leaf leaf-convert
        :load-path* "site-lisp"))

    ((leaf-convert
      (prog1 'leaf
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))))
     '(leaf leaf
        :load-path* "site-lisp"))

    ((leaf-convert
      (prog1 "leaf"
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))))
     '(leaf leaf
        :load-path* "site-lisp"))

    ((leaf-convert
      (with-eval-after-load 'leaf
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
        (declare-function leaf1 "leaf-1")
        (declare-function leaf2 "leaf-2")))
     '(leaf leaf
        :load-path* "site-lisp"
        :defun ((leaf2 . leaf-2)
                (leaf1 . leaf-1))
        :after t))))

(cort-deftest-with-equal leaf-convert/load-path
  '(((leaf-convert
      (add-to-list 'load-path "~/.emacs.d/local/26.3/site-lisp"))
     '(leaf leaf-convert
        :load-path "~/.emacs.d/local/26.3/site-lisp"))

    ((leaf-convert
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp")))
     '(leaf leaf-convert
        :load-path* "site-lisp"))

    ((leaf-convert
      (add-to-list 'load-path (concat user-emacs-directory "site-lisp")))
     '(leaf leaf-convert
        :load-path* "site-lisp"))))

(cort-deftest-with-equal leaf-convert/config
  '(((leaf-convert
      (leaf-keywords-init))
     '(leaf leaf-convert
        :config (leaf-keywords-init)))))

(cort-deftest-with-equal leaf-convert/defun
  '(((leaf-convert
      (declare-function leaf))
     '(leaf leaf-convert
        :defun leaf))

    ((leaf-convert
      (declare-function leaf "leaf"))
     '(leaf leaf-convert
        :defun (leaf . leaf)))))

(cort-deftest-with-equal leaf-convert/defvar
  '(((leaf-convert
      (defvar leaf-keywords))
     '(leaf leaf-convert
        :defvar leaf-keywords))))

;; (provide 'leaf-convert-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert-test.el ends here
