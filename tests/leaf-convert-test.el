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
  '(
    ;; leaf-covnert from nil generates empty leaf
    ((leaf-convert-from-contents
      nil)
     '(leaf leaf-convert))

    ;; leaf-convert--name accepts string
    ((leaf-convert-from-contents
      '((leaf-convert--name . "some-package")))
     '(leaf some-package))

    ;; leaf-convert--name also accepts symbol
    ((leaf-convert-from-contents
      '((leaf-convert--name . some-package)))
     '(leaf some-package))

    ;; leaf-convert could handle symbol t
    ((leaf-convert-from-contents
      '((disabled . (t))))
     '(leaf leaf-convert
        :disabled t))

    ;; leaf-convert could handle symbol nil
    ((leaf-convert-from-contents
      '((disabled . (nil))))
     '(leaf leaf-convert
        :disabled nil))

    ;; leaf-convert splice values
    ((leaf-convert-from-contents
      '((config . ((leaf-keywords-init)))))
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)))

    ;; leaf-convert splice values (in multi values)
    ((leaf-convert-from-contents
      '((config . ((leaf-keywords-init)
                   (leaf-keywords-teardown)))))
     '(leaf leaf-convert
        :config
        (leaf-keywords-teardown)
        (leaf-keywords-init)))))

(cort-deftest-with-equal leaf-convert/progn
  '(
    ;; accept progn
    ((leaf-convert
      (progn
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf"))
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-keywords"))))
     '(leaf leaf-convert
        :load-path*
        "site-lisp/leaf"
        "site-lisp/leaf-keywords"))

    ;; also accept prog1 and pick up 2th argument as leaf--name if symbol
    ((leaf-convert
      (prog1 'leaf
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf"))
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-keywords"))))
     '(leaf leaf
        :load-path*
        "site-lisp/leaf"
        "site-lisp/leaf-keywords"))

    ;; also accept prog1 and pick up 2th argument as leaf--name if string
    ((leaf-convert
      (prog1 "leaf"
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf"))
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-keywords"))))
     '(leaf leaf
        :load-path*
        "site-lisp/leaf"
        "site-lisp/leaf-keywords"))))

(cort-deftest-with-equal leaf-convert/load-path
  '(
    ;; add-to-list load-path convert to :load-path keyword
    ((leaf-convert
      (add-to-list 'load-path "~/.emacs.d/local/26.3/site-lisp"))
     '(leaf leaf-convert
        :load-path "~/.emacs.d/local/26.3/site-lisp"))

    ;; add-to-list load-path using locate-user-emacs-file convert to :load-path*
    ((leaf-convert
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp")))
     '(leaf leaf-convert
        :load-path* "site-lisp"))

    ;; add-to-list load-path using concat user-emacs-directory convert to :load-path*
    ((leaf-convert
      (add-to-list 'load-path (concat user-emacs-directory "site-lisp")))
     '(leaf leaf-convert
        :load-path* "site-lisp"))

    ;; could convert multi add-to-list sexps
    ((leaf-convert
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf"))
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-keywords"))
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-convert")))
     '(leaf leaf-convert
        :load-path*
        "site-lisp/leaf"
        "site-lisp/leaf-keywords"
        "site-lisp/leaf-convert"))))

(cort-deftest-with-equal leaf-convert/config
  '(
    ;; unknown sexp convert to :config
    ((leaf-convert
      (leaf-keywords-init))
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)))))

(cort-deftest-with-equal leaf-convert/defun
  '(
    ;; declare-function convert to :defun
    ((leaf-convert
      (declare-function leaf "leaf"))
     '(leaf leaf-convert
        :defun (leaf . leaf)))))

(cort-deftest-with-equal leaf-convert/defvar
  '(
    ;; empty defvar convert to :defvar
    ((leaf-convert
      (defvar leaf-keywords))
     '(leaf leaf-convert
        :defvar leaf-keywords))

    ;; define variable and initialize convert to :setq
    ((leaf-convert
      (defvar leaf-keywords-optional '(:doc :url :tag)))
     '(leaf leaf-convert
        :setq (leaf-keywords-optional . '(:doc :url :tag))))))

(cort-deftest-with-equal leaf-convert/after
  '(
    ;; eval-after-load convert to :after
    ((leaf-convert
      (eval-after-load 'leaf
        '(progn
           (leaf-browser-init))))
     '(leaf leaf
        :after leaf
        :config
        (leaf-browser-init)))

    ;; with-eval-after-load also convert to :after
    ((leaf-convert
      (with-eval-after-load 'leaf
        (leaf-browser-init)))
     '(leaf leaf
        :after leaf
        :config
        (leaf-browser-init)))

    ;; eval-after-load chain convert to :after symbols
    ((leaf-convert
      (eval-after-load 'orglyth
        '(eval-after-load 'org
           '(eval-after-load 'leaf
              '(progn
                 (leaf-browser-init))))))
     '(leaf leaf
        :after orglyth org leaf
        :config
        (leaf-browser-init)))

    ;; if the eval-after-load chain breaks, it will not be converted to the :after keyword
    ((leaf-convert
      (eval-after-load 'orglyth
        '(progn
           (orglyth-setup)
           (eval-after-load 'org
             '(eval-after-load 'leaf
                '(progn
                   (leaf-browser-init)))))))
     '(leaf orglyth
        :after orglyth
        :config
        (orglyth-setup)
        (eval-after-load 'leaf
          '(progn
             (leaf-browser-init)))))))

(cort-deftest-with-equal leaf-convert/setq
  '(
    ;; empty defvar convert to :defvar
    ((leaf-convert
      (defvar leaf-keywords))
     '(leaf leaf-convert
        :defvar leaf-keywords))

    ;; define variable and initialize convert to :setq
    ((leaf-convert
      (defvar leaf-keywords-optional '(:doc :url :tag)))
     '(leaf leaf-convert
        :setq (leaf-keywords-optional . '(:doc :url :tag))))))

;; (provide 'leaf-convert-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert-test.el ends here
