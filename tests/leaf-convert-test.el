;;; leaf-convert-test.el --- Test definitions for leaf-convert  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf-convert.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `leaf-convert'.


;;; Code:

(require 'buttercup)
(require 'leaf-convert)

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

(describe "Could convert from leaf-convert-contents"
  (it "Name"
    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new))
     :to-equal
     '(leaf leaf-convert))

    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :name "some-package"))
     :to-equal
     '(leaf some-package))

    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :name 'some-package))
     :to-equal
     '(leaf some-package)))

  (it ":disabled"
    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :disabled t))
     :to-equal
     '(leaf leaf-convert
        :disabled t))

    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :disabled :leaf-convert--nil))
     :to-equal
     '(leaf leaf-convert
        :disabled nil)))

  (it ":config"
    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :config '((leaf-keywords-init))))
     :to-equal
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)))

    (expect
     (leaf-convert-contents
      (leaf-convert-contents-new
       :config '((leaf-keywords-init)
                 (leaf-keywords-teardown))))
     :to-equal
     '(leaf leaf-convert
        :config
        (leaf-keywords-init)
        (leaf-keywords-teardown)))))

(describe "Leaf-convert-contents could convert from sexp"
  (it ":load-path"
    (expect
     (leaf-convert-contents-new--sexp
      (add-to-list 'load-path "~/.emacs.d/local/26.3/site-lisp"))
     :to-equal
     (leaf-convert-contents-new
      :load-path "~/.emacs.d/local/26.3/site-lisp"))

    (expect
     (leaf-convert-contents-new--sexp
      (add-to-list 'load-path (locate-user-emacs-file "site-lisp")))
     :to-equal
     (leaf-convert-contents-new
      :load-path* "site-lisp"))

    (expect
     (leaf-convert-contents-new--sexp
      (add-to-list 'load-path (concat user-emacs-directory "site-lisp")))
     :to-equal
     (leaf-convert-contents-new
      :load-path* "site-lisp")))

  (it ":config"
    (expect
     (leaf-convert-contents-new--sexp
      (leaf-keywords-init))
     :to-equal
     (leaf-convert-contents-new
      :config '((leaf-keywords-init)))))

  (it ":defun"
    (expect
     (leaf-convert-contents-new--sexp
      (declare-function leaf))
     :to-equal
     (leaf-convert-contents-new
      :defun 'leaf))

    (expect
     (leaf-convert-contents-new--sexp
      (declare-function leaf "leaf"))
     :to-equal
     (leaf-convert-contents-new
      :defun '(leaf . leaf))))

  (it ":defvar"
    (expect
     (leaf-convert-contents-new--sexp
      (defvar leaf-keywords))
     :to-equal
     (leaf-convert-contents-new
      :defvar 'leaf-keywords)))

  (it "progn support"
    (expect
     (leaf-convert-contents-new--sexp
      (progn
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
        (declare-function leaf1 "leaf-1")
        (declare-function leaf2 "leaf-2")))
     :to-equal
     (leaf-convert-contents-new
      :load-path* "site-lisp"
      :defun '((leaf2 . leaf-2)
               (leaf1 . leaf-1))))

    (expect
     (leaf-convert-contents-new--sexp
      (prog1 'leaf
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
        (declare-function leaf1 "leaf-1")
        (declare-function leaf2 "leaf-2")))
     :to-equal
     (leaf-convert-contents-new
      :name 'leaf
      :load-path* "site-lisp"
      :defun '((leaf2 . leaf-2)
               (leaf1 . leaf-1))))

    (expect
     (leaf-convert-contents-new--sexp
      (prog1 "leaf"
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
        (declare-function leaf1 "leaf-1")
        (declare-function leaf2 "leaf-2")))
     :to-equal
     (leaf-convert-contents-new
      :name "leaf"
      :load-path* "site-lisp"
      :defun '((leaf2 . leaf-2)
               (leaf1 . leaf-1))))

    (expect
     (leaf-convert-contents-new--sexp
      (with-eval-after-load 'leaf
        (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
        (declare-function leaf1 "leaf-1")
        (declare-function leaf2 "leaf-2")))
     :to-equal
     (leaf-convert-contents-new
      :name 'leaf
      :after t
      :load-path* "site-lisp"
      :defun '((leaf2 . leaf-2)
               (leaf1 . leaf-1))))))

;; (provide 'leaf-convert-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert-test.el ends here
