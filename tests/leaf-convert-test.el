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

(cort-deftest-with-equal leaf-convert/use-package--getting-started
  '(
    ;;; Getting started
    ;;; https://github.com/jwiegley/use-package#

    ;; simplest use-package
    ((leaf-convert-from-use-package
      (use-package foo))
     '(leaf foo
        :require t))

    ;; :init keyword
    ((leaf-convert-from-use-package
      (use-package foo
        :init
        (setq foo-variable t)))
     '(leaf foo
        :pre-setq ((foo-variable . t))
        :require t))

    ;; :config keyword
    ((leaf-convert-from-use-package
      (use-package foo
        :init
        (setq foo-variable t)
        :config
        (foo-mode 1)))
     '(leaf foo
        :pre-setq ((foo-variable . t))
        :require t
        :config (foo-mode 1)))

    ;; :comands keyword
    ((leaf-convert-from-use-package    ; TODO
      (use-package color-moccur
        :commands isearch-moccur
        :config
        (use-package moccur-edit)))
     '(leaf color-moccur
        :commands isearch-moccur
        :config
        (eval-after-load 'color-moccur
          '(progn
             (use-package moccur-edit)
             t))))

    ;; init, :config, :bind
    ((leaf-convert-from-use-package
      (use-package color-moccur
        :commands (isearch-moccur isearch-all)
        :bind (("M-s O" . moccur)
               :map isearch-mode-map
               ("M-o" . isearch-moccur)
               ("M-O" . isearch-moccur-all))
        :init
        (setq isearch-lazy-highlight t)
        :config
        (use-package moccur-edit)))
     '(leaf color-moccur
        :commands moccur isearch-moccur isearch-moccur-all isearch-all
        :bind (("M-s O" . moccur)
               (isearch-mode-map
                ("M-o" . isearch-moccur)
                ("M-O" . isearch-moccur-all)))
        :config
        (eval-after-load 'color-moccur
          '(progn (use-package moccur-edit) t))
        :setq ((isearch-lazy-highlight . t))))))

(cort-deftest-with-equal leaf-convert/use-package--Keybinding
  '(
    ((leaf-convert-from-use-package
      (use-package ace-jump-mode
        :bind ("C-." . ace-jump-mode)))
     '(leaf ace-jump-mode
        :commands ace-jump-mode
        :bind (("C-." . ace-jump-mode))))

    ((leaf-convert-from-use-package
      (use-package hi-lock
        :bind (("M-o l" . highlight-lines-matching-regexp)
               ("M-o r" . highlight-regexp)
               ("M-o w" . highlight-phrase))))
     '(leaf hi-lock
        :commands highlight-lines-matching-regexp highlight-regexp highlight-phrase
        :bind (("M-o l" . highlight-lines-matching-regexp)
               ("M-o r" . highlight-regexp)
               ("M-o w" . highlight-phrase))))

    ((leaf-convert-from-use-package
      (use-package helm
        :bind (("M-x" . helm-M-x)
               ("M-<f5>" . helm-find-files)
               ([f10] . helm-buffers-list)
               ([S-f10] . helm-recentf))))
     '(leaf helm
        :commands helm-M-x helm-find-files helm-buffers-list helm-recentf
        :bind (("M-x" . helm-M-x)
               ("M-<f5>" . helm-find-files)
               ([f10] . helm-buffers-list)
               ([S-f10] . helm-recentf))))

    ((leaf-convert-from-use-package
      (use-package unfill
        :bind ([remap fill-paragraph] . unfill-toggle)))
     '(leaf unfill
        :commands unfill-toggle
        :bind (([remap fill-paragraph] . unfill-toggle))))

    ((leaf-convert-from-use-package
      (use-package helm
        :bind (:map helm-command-map
                    ("C-c h" . helm-execute-persistent-action))))
     '(leaf helm
        :commands helm-execute-persistent-action
        :bind ((helm-command-map
                ("C-c h" . helm-execute-persistent-action)))))

    ((leaf-convert-from-use-package
      (use-package term
        :bind (("C-c t" . term)
               :map term-mode-map
               ("M-p" . term-send-up)
               ("M-n" . term-send-down)
               :map term-raw-map
               ("M-o" . other-window)
               ("M-p" . term-send-up)
               ("M-n" . term-send-down))))
     '(leaf term
        :commands term term-send-up term-send-down other-window
        :bind (("C-c t" . term)
               (term-mode-map
                ("M-p" . term-send-up)
                ("M-n" . term-send-down))
               (term-raw-map
                ("M-o" . other-window)
                ("M-p" . term-send-up)
                ("M-n" . term-send-down)))))))

(cort-deftest-with-equal leaf-convert/use-package--modes-and-interpreters
  '(
    ((leaf-convert-from-use-package
      (use-package ruby-mode
        :mode "\\.rb\\'"
        :interpreter "ruby"))
     '(leaf ruby-mode
        :commands ruby-mode
        :mode (("\\.rb\\'" . ruby-mode))
        :interpreter (("ruby" . ruby-mode))))

    ((leaf-convert-from-use-package
      (use-package python
        :mode ("\\.py\\'" . python-mode)
        :interpreter ("python" . python-mode)))
     '(leaf python
        :commands python-mode
        :mode (("\\.py\\'" . python-mode))
        :interpreter (("python" . python-mode))))))

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
        :setq ((leaf-keywords-optional . '(:doc :url :tag)))))))

(cort-deftest-with-equal leaf-convert/after
  '(
    ;; eval-after-load convert to :after
    ((leaf-convert
      (eval-after-load 'leaf
        '(progn
           (leaf-browser-init))))
     '(leaf leaf
        :after t
        :config
        (leaf-browser-init)))

    ;; with-eval-after-load also convert to :after
    ((leaf-convert
      (with-eval-after-load 'leaf
        (leaf-browser-init)))
     '(leaf leaf
        :after t
        :config
        (leaf-browser-init)))

    ;; eval-after-load chain convert to :after symbols
    ((leaf-convert
      (eval-after-load 'orglyth
        '(eval-after-load 'org
           '(eval-after-load 'leaf
              '(progn
                 (leaf-browser-init))))))
     '(leaf orglyth
        :after t org leaf
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
        :after t
        :config
        (orglyth-setup)
        (eval-after-load 'org
          '(eval-after-load 'leaf
             '(progn (leaf-browser-init))))))))

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
        :setq ((leaf-keywords-optional . '(:doc :url :tag)))))

    ;; setq sexp convert to :setq keyword
    ((leaf-convert
      (prog1 'alloc
        (setq gc-cons-threshold 536870912)
        (setq garbage-collection-messages t)))
     '(leaf alloc
        :setq ((gc-cons-threshold . 536870912)
               (garbage-collection-messages . t))))

    ;; right value is non-atom, convert to :config
    ((leaf-convert
      (prog1 'alloc
        (setq gc-cons-threshold (* 512 1024 1024))
        (setq garbage-collection-messages t)))
     '(leaf alloc
        :config (setq gc-cons-threshold (* 512 1024 1024))
        :setq ((garbage-collection-messages . t))))))

(cort-deftest-with-equal leaf-convert/setq-default
  '(
    ;; setq-default sexp convert to :setq-default keyword
    ((leaf-convert
      (prog1 'alloc
        (setq-default gc-cons-threshold 536870912)
        (setq-default garbage-collection-messages t)))
     '(leaf alloc
        :setq-default ((gc-cons-threshold . 536870912)
                       (garbage-collection-messages . t))))

    ;; right value is non-atom, convert to :config
    ((leaf-convert
      (prog1 'alloc
        (setq-default gc-cons-threshold (* 512 1024 1024))
        (setq-default garbage-collection-messages t)))
     '(leaf alloc
        :config (setq-default gc-cons-threshold (* 512 1024 1024))
        :setq-default ((garbage-collection-messages . t))))))

(cort-deftest-with-equal leaf-convert/diminish
  '(
    ;; hide minor-mode lighter
    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode)))
     '(leaf rainbow-mode
        :diminish rainbow-mode))

    ;; right value is string, converted cons-cell
    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode "Rbow")))
     '(leaf rainbow-mode
        :diminish (rainbow-mode . "Rbow")))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode 'rainbow-mode-lighter)))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote rainbow-mode-lighter)))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '(" " "R-" "bow"))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote (" " "R-" "bow"))))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '((" " "R-") "/" "bow"))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote ((" " "R-") "/" "bow"))))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '(:eval (format " Rbow/%s" (+ 2 3))))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote (:eval (format " Rbow/%s" (+ 2 3))))))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '(:propertize " Rbow" face '(:foreground "green")))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote (:propertize " Rbow" face '(:foreground "green")))))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '(rainbow-mode-mode-linep " Rbow/t" " Rbow/nil"))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote (rainbow-mode-mode-linep " Rbow/t" " Rbow/nil"))))

    ((leaf-convert
      (prog1 'rainbow-mode
        (diminish 'rainbow-mode '(2 " Rbow" "/" "s"))))
     '(leaf rainbow-mode
        :diminish (rainbow-mode quote (2 " Rbow" "/" "s"))))))

(cort-deftest-with-equal leaf-convert/ensure
  '(
    ;; package-install will convert :ensure keyword
    ((leaf-convert
      (prog1 'leaf
        (package-install 'leaf)))
     '(leaf leaf
        :ensure t))

    ;; could convert use-package :ensure t
    ((leaf-convert-from-use-package
      (use-package leaf
        :ensure t))
     '(leaf leaf
        :ensure t
        :require t))

    ;; could convert use-package :ensure argument
    ((leaf-convert-from-use-package
      (use-package tex
        :ensure auctex))
     '(leaf tex
        :ensure auctex
        :require t))

    ;; could convert use-package :ensure arguments
    ((leaf-convert-from-use-package
      (use-package tex
        :ensure t
        :ensure auctex))
     '(leaf tex
        :ensure t auctex
        :require t))))

(cort-deftest-with-equal leaf-convert/require
  '(
    ;; require will convert :require keyword
    ((leaf-convert
      (prog1 'leaf
        (require 'leaf)))
     '(leaf leaf
        :require t))

    ;; require with no-error will convert :require keyword
    ((leaf-convert
      (prog1 'leaf
        (require 'leaf nil t)))
     '(leaf leaf
        :require t))

    ;; empty use-package will convert :require keyword
    ((leaf-convert-from-use-package
      (use-package tex))
     '(leaf tex
        :require t))

    ;; if second argument is non-nil, cannot convert :require keyword
    ((leaf-convert
      (prog1 'leaf
        (require 'leaf "~/.emacs.d/site-lisp/leaf.el/leaf.el" t)))
     '(leaf leaf
        :config
        (require 'leaf "~/.emacs.d/site-lisp/leaf.el/leaf.el" t)))

    ;; Sexps that are executed before require are placed in the appropriate place
    ((leaf-convert-from-use-package
      (use-package foo
        :init
        (setq foo-variable t)
        (foo-init)
        :config
        (foo-enable)))
     '(leaf foo
        :init (foo-init)
        :pre-setq ((foo-variable . t))
        :require t
        :config (foo-enable)))))

(cort-deftest-with-equal leaf-convert/commands
  '(
    ;; autoload will convert :commands keyword
    ((leaf-convert
      (autoload #'moccur "color-moccur"))
     '(leaf leaf-convert
        :commands moccur))

    ;; autoload second argumemnt may be symbol
    ((leaf-convert
      (autoload #'moccur 'color-moccur))
     '(leaf leaf-convert
        :commands moccur))

    ;; autoload third, fourth argument are ignored
    ((leaf-convert
      (autoload #'moccur 'color-moccur nil t))
     '(leaf leaf-convert
        :commands moccur))

    ;; unless-autoload pattern also converted :commands
    ((leaf-convert
      (unless (fboundp 'moccur)
        (autoload #'moccur "color-moccur" nil t)))
     '(leaf leaf-convert
        :commands moccur))

    ;; use-package :commands converted :commands
    ((leaf-convert-from-use-package
      (use-package color-moccur
        :commands (isearch-moccur isearch-all)
        :init
        (setq isearch-lazy-highlight t)))
     '(leaf color-moccur
        :commands isearch-moccur isearch-all
        :setq ((isearch-lazy-highlight . t))))))

(cort-deftest-with-equal leaf-convert/bind
  '(
    ;; global-set-key convert :bind keyword
    ((leaf-convert
      (prog1 'simple
        (global-set-key (kbd "C-h") 'delete-backward-char)))
     '(leaf simple
        :bind (("C-h" . delete-backward-char))))

    ;; global-unset-key convert :bind keyword
    ((leaf-convert
      (prog1 'simple
        (global-unset-key (kbd "M-o"))))
     '(leaf simple
        :bind (("M-o"))))

    ;; define-key to global-map convert :bind keyword
    ((leaf-convert
      (prog1 'simple
        (define-key global-map (kbd "C-h") 'delete-backward-char)))
     '(leaf simple
        :bind (("C-h" . delete-backward-char))))

    ;; define-key nil to unset keybind
    ((leaf-convert
      (prog1 'simple
        (define-key global-map (kbd "C-h") nil)))
     '(leaf simple
        :bind (("C-h" . nil))))

    ;; define-key to specific keymap convert :bind keyword
    ((leaf-convert
      (prog1 'dired
        (define-key dired-mode-map (kbd "C-t") nil)))
     '(leaf dired
        :bind ((dired-mode-map
                ("C-t" . nil)))))

    ;; define-key remap convert :bind keyword
    ((leaf-convert
      (prog1 'dired
        (define-key dired-mode-map [remap next-line] 'dired-next-line)))
     '(leaf dired
        :bind ((dired-mode-map
                ([remap next-line] . dired-next-line)))))

    ;; bind-key convert :bind keyword
    ((leaf-convert
      (prog1 'simple
        (bind-key "C-h" 'delete-backward-char)))
     '(leaf simple
        :bind (("C-h" . delete-backward-char))))

    ;; bind-key specific keymap convert :bind keyword
    ((leaf-convert
      (prog1 'simple
        (bind-key "C-h" 'delete-backward-char prog-mode-map)))
     '(leaf simple
        :bind ((prog-mode-map
                ("C-h" . delete-backward-char)))))

    ;; simple bind-keys convert :bind keyword
    ((leaf-convert
      (prog1 'dired
        (bind-keys :map dired-mode-map
                   ("o" . dired-omit-mode)
                   ("a" . some-custom-dired-function))))
     '(leaf dired
        :bind ((dired-mode-map
                ("o" . dired-omit-mode)
                ("a" . some-custom-dired-function)))))))

(cort-deftest-with-equal leaf-convert/bind*
  '(
    ;; bind-key* convert :bind* keyword
    ((leaf-convert
      (prog1 'simple
        (bind-key* "C-h" 'delete-backward-char)))
     '(leaf simple
        :bind* (("C-h" . delete-backward-char))))))

(cort-deftest-with-equal leaf-convert/mode
  '(
    ;; add-to-list 'auto-mode-alist to :mode keyword
    ((leaf-convert
      (prog1 'ruby-mode
        (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))))
     '(leaf ruby-mode
        :mode (("\\.rb\\'" . ruby-mode))))))

(cort-deftest-with-equal leaf-convert/interpreter
  '(
    ;; add-to-list 'interpreter-alist to :interpreter keyword
    ((leaf-convert
      (prog1 'ruby-mode
        (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))))
     '(leaf ruby-mode
        :interpreter (("ruby" . ruby-mode))))))

(cort-deftest-with-equal leaf-convert/magic
  '(
    ;; add-to-list 'magic-mode-alist to :magic keyword
    ((leaf-convert
      (prog1 'pdf-tools
        (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))))
     '(leaf pdf-tools
        :magic (("%PDF" . pdf-view-mode))))))

(cort-deftest-with-equal leaf-convert/magic-fallback
  '(
    ;; add-to-list 'magic-fallback-mode-alist to :magic-fallback keyword
    ((leaf-convert
      (prog1 'pdf-tools
        (add-to-list 'magic-fallback-mode-alist '("%PDF" . pdf-view-mode))))
     '(leaf pdf-tools
        :magic-fallback (("%PDF" . pdf-view-mode))))))

;; (provide 'leaf-convert-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert-test.el ends here
