;;; leaf-convert.el --- Convert many format to leaf format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1") (leaf "3.6.0") (leaf-keywords "1.1.0") (use-package "2.4"))
;; URL: https://github.com/conao3/leaf-convert.el

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

;; Convert many format to leaf format.


;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package)
(require 'lisp-mnt)
(require 'leaf)
(require 'leaf-keywords)
(require 'use-package)

(defgroup leaf-convert nil
  "Convert many format to leaf format."
  :prefix "leaf-convert-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-convert.el"))

(defcustom leaf-convert-prefer-list-keywords
  (leaf-list
   :bind :bind* :mode :interpreter :magic :magic-fallback :hook
   :custom :custom* :custom-face :pl-custom :auth-custom
   :setq :pre-setq :setq-default
   :pl-setq :pl-pre-setq :pl-setq-default
   :auth-setq :auth-pre-setq :auth-setq-defualt)
  "Prefer list output keywords."
  :group 'leaf-convert
  :type 'sexp)

(defcustom leaf-convert-leaf-name-omittable-keywords
  (leaf-list
   :ensure :feather :package :require :after)
  "Keywords that interpret t as leaf--name."
  :group 'leaf-convert
  :type 'sexp)

(defcustom leaf-convert-except-after
  (leaf-list
   cl-lib let-alist pkg-info json seq
   dash dash-functional s f ht ov
   bind-key async promise async-await
   memoise popup popwin request simple-httpd transient htmlize)
  "Except packages for :after keyword.
see `leaf-convert--fill-info'"
  :group 'leaf-convert
  :type 'sexp)


;;; Functions
(defun leaf-convert--mode-line-structp (elm)
  "Return non-nil if ELM is varid mode-line structure.
See https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html"
  (or (stringp elm)
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (atom (cadr elm)))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (stringp (car (cadr elm))))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (listp (car (cadr elm))))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (eq :eval (car (cadr elm))))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (eq :propertize (car (cadr elm))))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (symbolp (car (cadr elm))))
      (and (= 2 (safe-length elm))
           (eq 'quote (car elm))
           (listp (cadr elm))
           (integerp (car (cadr elm))))))

(defun leaf-convert--symbol-from-string (elm)
  "Convert ELM to symbol.  If ELM is nil, return DEFAULT.
ELM can be string or symbol."
  (if (stringp elm) (intern elm) elm))

(defun leaf-convert-contents--parse-bind-keys (op bind-keys-args contents)
  "Parse bind-keys argument and push values to CONTENTS.
OP is bind-keys or bind-keys* symbol.
BIND-KEYS-ARGS is bind-keys' all argument."
  (let* ((keyword (pcase op ('bind-keys 'bind) ('bind-keys* 'bind*)))
         (leaf-keys-args (alist-get keyword contents))
         keyalist mapalist
         rawargs unknownfrg)
    (setq rawargs bind-keys-args)
    (while leaf-keys-args
      (pcase (pop leaf-keys-args)
        (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)))
         (push `(,key . nil) (alist-get 'global-map keyalist)))
        (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)) . ,fn)
         (push `(,key . ,fn) (alist-get 'global-map keyalist)))
        (`(,(and (pred symbolp) map) :package ,(and (pred symbolp) pkg) . ,cells)
         (when-let (p (alist-get map mapalist))
           (unless (eq p pkg)
             (setq unknownfrg t)))
         (setf (alist-get map mapalist) pkg)
         (dolist (cell cells)
           (pcase cell
             (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)))
              (push `(,key . nil) (alist-get map keyalist)))
             (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)) . ,fn)
              (push `(,key . ,fn) (alist-get map keyalist)))
             (_
              (setq unknownfrg t)))))
        (`(,(and (pred symbolp) map) . ,cells)
         (dolist (cell cells)
           (pcase cell
             (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)))
              (push `(,key . nil) (alist-get map keyalist)))
             (`(,(or `,(and (pred stringp) key) `,(and (pred vectorp) key)) . ,fn)
              (push `(,key . ,fn) (alist-get map keyalist)))
             (_
              (setq unknownfrg t)))))
        (_
         (setq unknownfrg t))))
    (let ((map 'global-map) pkg)
      (while bind-keys-args
        (pcase (pop bind-keys-args)
          ((or :prefix :prefix-map)
           (setq unknownfrg t))
          (:map
           (setq map (pop bind-keys-args)))
          (:package
           (setq pkg (pop bind-keys-args))
           (unless (eq 'global-map map)
             (when-let (p (alist-get map mapalist))
               (unless (eq p pkg)
                 (setq unknownfrg t)))
             (setf (alist-get map mapalist) pkg)))
          (`(,key . ,(and (pred symbolp) fn))
           (push `(,key . ,fn) (alist-get map keyalist)))
          (_
           (setq unknownfrg t)))))
    (if (and (or unknownfrg (not keyword)) rawargs)
        (push `(,op ,@rawargs) (alist-get 'config contents))
      (let (tmp)
        (setq tmp `(,@(let (lst)
                        (pcase-dolist (`(,map . ,keys) keyalist)
                          (unless (eq 'global-map map)
                            (if-let ((mappkg (alist-get map mapalist)))
                                (setf (alist-get map lst) `(:package ,mappkg ,@(nreverse (delete-dups keys))))
                              (setf (alist-get map lst) (nreverse (delete-dups keys))))))
                        (nreverse lst))
                    ,@(delete-dups (alist-get 'global-map keyalist))))
        (setf (alist-get keyword contents) tmp)))
    contents))

(defun leaf-convert-contents-new--sexp-1 (sexp contents)
  "Internal recursive function of `leaf-convert-contents-new--sexp'.
Add convert SEXP to leaf-convert-contents to CONTENTS."
  (cl-flet ((constp (elm) (or (atom elm)
                              (member (car elm) '(quote function))))
            (fnp (elm) (or (null elm)
                           (and (= 2 (safe-length elm))
                                (member (car elm) '(quote function))))))
    (pcase sexp
      ;; :load-path, :load-path*
      (`(add-to-list 'load-path ,(and (pred stringp) elm))
       (push elm (alist-get 'load-path contents)))
      (`(add-to-list 'load-path (locate-user-emacs-file ,(and (pred stringp) elm)))
       (push elm (alist-get 'load-path* contents)))
      (`(add-to-list 'load-path (concat user-emacs-directory ,(and (pred stringp) elm)))
       (push elm (alist-get 'load-path* contents)))
      (`(eval-and-compile (add-to-list 'load-path ,(and (pred stringp) elm))) ; use-package's :load-path
       (let ((relpath (file-relative-name elm user-emacs-directory)))
         (if (not (string-match "\\.\\./" relpath))
             (push relpath (alist-get 'load-path* contents))
           (push elm (alist-get 'load-path contents)))))

      ;; :defun
      (`(declare-function ,(and (pred atom) elm) ,(and (pred stringp) file) . ,_args)
       (push `(,elm . ,(intern file)) (alist-get 'defun contents)))

      ;; :defvar
      (`(defvar ,(and (pred atom) elm))
       (push elm (alist-get 'defvar contents)))
      (`(defvar ,(and (pred atom) elm) ,(and (pred constp) val))
       (push `(,elm . ,val) (alist-get 'setq contents)))

      ;; :ensure
      (`(package-install ',(and (pred symbolp) elm))
       (push elm (alist-get 'ensure contents)))
      (`(use-package-ensure-elpa ',(and (pred symbolp) elm) ',val 'nil)
       (dolist (v val)
         (if (eq t v)
             (push elm (alist-get 'ensure contents))
           (push v (alist-get 'ensure contents)))))

      ;; :commands
      (`(autoload ,(or `',(and (pred symbolp) elm) `#',(and (pred symbolp) elm)) . ,_args)
       (push elm (alist-get 'commands contents)))
      (`(unless (fboundp ,(or `',(and (pred symbolp) fn) `#',(and (pred symbolp) fn)))
          (autoload ,(or `',(and (pred symbolp) elm) `#',(and (pred symbolp) elm)) . ,_args))
       (if (eq fn elm)
           (push elm (alist-get 'commands contents))
         (push sexp (alist-get 'config contents))))

      ;; :bind, :bind*
      (`(global-unset-key ,(or `(kbd ,key) `,(and (pred vectorp) key)))
       (push `(,key . nil) (alist-get 'bind contents)))
      (`(global-set-key ,(or `(kbd ,key) `,(and (pred vectorp) key)) ,(and (pred fnp) fn))
       (push `(,key . ,(cadr fn)) (alist-get 'bind contents)))
      (`(define-key global-map ,(or `(kbd ,key) `,(and (pred vectorp) key)) ,(and (pred fnp) fn))
       (push `(,key . ,(cadr fn)) (alist-get 'bind contents)))
      (`(define-key ,(and (pred symbolp) map) ,(or `(kbd ,key) `,(and (pred vectorp) key)) ,(and (pred fnp) fn))
       (push `(,map (,key . ,(cadr fn))) (alist-get 'bind contents)))
      (`(bind-key ,key ,(and (pred fnp) fn))
       (push `(,key . ,(cadr fn)) (alist-get 'bind contents)))
      (`(bind-key ,key ,(and (pred fnp) fn) ,(and (pred symbolp) map))
       (push `(,map (,key . ,(cadr fn))) (alist-get 'bind contents)))
      (`(bind-key* ,key ,(and (pred fnp) fn))
       (push `(,key . ,(cadr fn)) (alist-get 'bind* contents)))
      (`(,(and (or 'bind-keys 'bind-keys*) op) . ,args)
       (setq contents (leaf-convert-contents--parse-bind-keys op args contents)))

      ;; :mode, :interpreter, :magic, :magic-fallback
      (`(add-to-list 'auto-mode-alist '(,(and (pred stringp) elm) . ,(and (pred symbolp) fn)))
       (push `(,elm . ,fn) (alist-get 'mode contents)))
      (`(add-to-list 'interpreter-mode-alist '(,(and (pred stringp) elm) . ,(and (pred symbolp) fn)))
       (push `(,elm . ,fn) (alist-get 'interpreter contents)))
      (`(add-to-list 'magic-mode-alist '(,(and (pred stringp) elm) . ,(and (pred symbolp) fn)))
       (push `(,elm . ,fn) (alist-get 'magic contents)))
      (`(add-to-list 'magic-fallback-mode-alist '(,(and (pred stringp) elm) . ,(and (pred symbolp) fn)))
       (push `(,elm . ,fn) (alist-get 'magic-fallback contents)))

      ;; :hook
      (`(add-hook ',(and (pred symbolp) elm) ,(or `',(and (pred symbolp) fn) `#',(and (pred symbolp) fn)))
       (push `(,elm . ,fn) (alist-get 'hook contents)))
      (`(add-hook ',(and (pred symbolp) elm) ,(or `',(and (pred symbolp) fn) `#',(and (pred symbolp) fn)) ,_append)
       (push `(,elm . ,fn) (alist-get 'hook contents)))

      ;; :custom, :custom*
      (`(customize-set-variable ',(and (pred symbolp) elm) ,(and (pred constp) val))
       (push `(,elm . ,val) (alist-get 'custom contents)))
      (`(customize-set-variable ',(and (pred symbolp) elm) ,(and (pred constp) val) ,(pred (string-match "^Customized with use-package")))
       (push `(,elm . ,val) (alist-get 'custom contents)))
      (`(customize-set-variable ',(and (pred symbolp) elm) ,(and (pred constp) val) ,(and (pred stringp) desc))
       (push `(,elm ,val ,desc) (alist-get 'custom* contents)))
      (`(custom-set-variables . ,args)
       (pcase-dolist (`',elm args)
         (setq contents (leaf-convert-contents-new--sexp-1 `(customize-set-variable ',(car elm) ,@(cdr elm)) contents))))

      ;; :custom-face
      (`(custom-set-faces . ,args)
       (pcase-dolist (`',elm args)
         (if (memq '\, (leaf-flatten args)) ; use-package accept right value include comma
             (push `(custom-set-faces (backquote (,(car elm) ,(cadr elm)))) (alist-get 'config contents))
           (push `(,(car elm) . ',(cadr elm)) (alist-get 'custom-face contents)))))

      ;; :require
      (`(require ',(and (pred symbolp) elm))
       (progn                           ; move :config sexp to :init section
         (setf (alist-get 'pre-setq contents) (alist-get 'setq contents)) (setf (alist-get 'setq contents nil 'remove) nil)
         (setf (alist-get 'init contents) (alist-get 'config contents)) (setf (alist-get 'config contents nil 'remove) nil))
       (push elm (alist-get 'require contents)))
      (`(require ',(and (pred symbolp) elm) nil ,_)
       (progn                           ; move :config sexp to :init section
         (setf (alist-get 'pre-setq contents) (alist-get 'setq contents)) (setf (alist-get 'setq contents nil 'remove) nil)
         (setf (alist-get 'init contents) (alist-get 'config contents)) (setf (alist-get 'config contents nil 'remove) nil))
       (push elm (alist-get 'require contents)))

      ;; :diminish, :delight
      (`(,(and (or 'diminish 'delight) op) ',(and (pred symbolp) elm))
       (push elm (alist-get op contents)))
      (`(,(and (or 'diminish 'delight) op) ',(and (pred symbolp) elm) ,(and (pred leaf-convert--mode-line-structp) val))
       (push `(,elm . ,val) (alist-get op contents)))

      ;; :setq, :setq-default
      (`(,(and (or 'setq 'setq-default) op) ,(and (pred atom) elm) ,(and (pred constp) val))
       (push `(,elm . ,val) (alist-get op contents)))

      ;; any
      (_ (push sexp (alist-get 'config contents)))))
  contents)

(defun leaf-convert-contents-new--sexp-internal (sexp &optional contents toplevel)
  "Internal function of `leaf-convert-contents-new--sexp'.
Convert SEXP to leaf-convert-contents.
If specified CONTENTS, add value to it instead of create new instance.
When TOPLEVEL is non-nil, it converts sexp, which affects the
whole block like `eval-after-load', into leaf keyword.'"
  (pcase sexp
    ;; leaf--name, progn
    (`(prog1 ,(or `',name (and (pred stringp) name)) . ,body)
     (setf (alist-get 'leaf-convert--name contents) (leaf-convert--symbol-from-string name))
     (setq contents (leaf-convert-contents-new--sexp-internal `(progn ,@body) contents toplevel)))

    ;; progn
    (`(progn . ,body)
     (dolist (elm body)
       (unless (atom elm)
         (setq contents (leaf-convert-contents-new--sexp-internal elm contents (and toplevel (equal elm (car body))))))))

    ;; :when, :unless, :if
    (`(unless (fboundp ,(or `',(and (pred symbolp) fn) `#',(and (pred symbolp) fn))) ; use-pakage :commands idiom
        (autoload ,(or `',(and (pred symbolp) elm) `#',(and (pred symbolp) elm)) . ,_args))
     (if (eq fn elm)
         (push elm (alist-get 'commands contents))
       (push sexp (alist-get 'config contents))))
    (`(when (and . ,conditions) . ,body)
     (let ((toplevel* (or toplevel)))   ; TODO
       (if (not toplevel*)
           (push sexp (alist-get 'config contents))
         (dolist (condition conditions)
           (push condition (alist-get 'when contents)))
         (setq contents (leaf-convert-contents-new--sexp-internal `(progn ,@body) contents toplevel)))))
    (`(when ,condition . ,body)
     (let ((toplevel* (or toplevel)))   ; TODO
       (if (not toplevel*)
           (push sexp (alist-get 'config contents))
         (pcase condition
           (`(not (not (not (not (symbol-value ',val))))) (push val (alist-get 'when contents)))
           (`(not (not (not (symbol-value ',val))))       (push val (alist-get 'unless contents)))
           (`(not (not (symbol-value ',val)))             (push val (alist-get 'when contents)))
           (`(not (symbol-value ',val))                   (push val (alist-get 'unless contents)))
           (`(symbol-value ',val)                         (push val (alist-get 'when contents)))
           (`(not (not (not (not ,condition))))           (push condition (alist-get 'when contents)))
           (`(not (not (not ,condition)))                 (push condition (alist-get 'unless contents)))
           (`(not (not ,condition))                       (push condition (alist-get 'when contents)))
           (`(not ,condition)                             (push condition (alist-get 'unless contents)))
           (`,condition                                   (push condition (alist-get 'when contents))))
         (setq contents (leaf-convert-contents-new--sexp-internal `(progn ,@body) contents toplevel*)))))
    (`(unless ,condition . ,body)
     (setq contents (leaf-convert-contents-new--sexp-internal `(when (not ,condition) ,@body) contents toplevel)))
    (`(if ,condition ,body)
     (setq contents (leaf-convert-contents-new--sexp-internal `(when ,condition ,body) contents toplevel)))
    (`(if ,condition ,body nil)
     (setq contents (leaf-convert-contents-new--sexp-internal `(when ,condition ,body) contents toplevel)))
    (`(if ,condition nil . ,body)
     (setq contents (leaf-convert-contents-new--sexp-internal `(when (not ,condition) ,@body) contents toplevel)))
    (`(if . ,_body)
     (push sexp (alist-get 'config contents)))

    ;; leaf--name, :after
    (`(with-eval-after-load ,(or `',name (and (pred stringp) name))
        . ,body)
     (setq contents (leaf-convert-contents-new--sexp-internal `(eval-after-load ',name '(progn ,@body)) contents toplevel)))

    ;; leaf--name, :after
    (`(eval-after-load ,(or `',name (and (pred stringp) name))
        ',body)
     (let ((toplevel* (or toplevel)))   ; TODO
       (if (not toplevel*)
           (push sexp (alist-get 'config contents))
         (unless (alist-get 'leaf-convert--name contents)
           (setf (alist-get 'leaf-convert--name contents) (leaf-convert--symbol-from-string name)))
         (push name (alist-get 'after contents))
         (setq contents (leaf-convert-contents-new--sexp-internal body contents toplevel)))))

    ;; any
    (_
     (setq contents (leaf-convert-contents-new--sexp-1 sexp contents))))
  contents)

(defmacro leaf-convert-contents-new--sexp (sexp &optional contents)
  "Convert SEXP to leaf-convert-contents.
If specified CONTENTS, add value to it instead of new instance."
  `(leaf-convert-contents-new--sexp-internal ',sexp ,contents 'toplevel))

(defun leaf-convert--fill-info (contents)
  "Add :doc, :file, :url information to CONTENTS."
  ;; see `describe-package-1'
  (let* ((pkg (alist-get 'leaf-convert--name contents))
         (desc (when pkg
                 (or
                  (if (package-desc-p pkg) pkg)
                  (cadr (assq pkg package-alist))
                  (let ((built-in (assq pkg package--builtins)))
                    (if built-in
                        (package--from-builtin built-in)
                      (cadr (assq pkg package-archive-contents)))))))
         docs reqs tags files urls)
    (cond
     ((and (not pkg)))
     ((and pkg (not desc))
      (if-let (file (locate-file (format "%s.el" pkg) load-path load-file-rep-suffixes))
          (with-temp-buffer
            (insert-file-contents file)
            (push file files)
            (push (lm-summary) docs)
            (push (lm-homepage) urls)
            (dolist (keyword (lm-keywords-list)) (push keyword tags))
            (setq reqs (when-let (str (lm-header "package-requires"))
                         (mapcar #'(lambda (elt) `(,(car elt) ,(version-to-list (cadr elt))))
                                 (package--prepare-dependencies
                                  (package-read-from-string str)))))
            (if (or (package-built-in-p pkg) (string-match-p "/share/" file))
                (push "builtin" tags)
              (push "satellite" tags)
              (push (intern (format "{{user}}/%s" pkg)) (alist-get 'el-get contents))))
        (push "satellite" tags)
        (push (intern (format "{{user}}/%s" pkg)) (alist-get 'el-get contents))))
     ((and pkg desc)
      (push (package-desc-summary desc) docs)
      (push (locate-file (format "%s.el" pkg) load-path load-file-rep-suffixes) files)
      (push (cdr (assoc :url (package-desc-extras desc))) urls)
      (dolist (keyword (package-desc--keywords desc)) (push keyword tags))
      (setq reqs (package-desc-reqs desc))
      (if (package-built-in-p desc)
          (push "builtin" tags)
        (push t (alist-get 'ensure contents)))))

    (push (format-time-string "%Y-%m-%d") (alist-get 'added contents))
    (dolist (doc docs)   (when doc (push doc (alist-get 'doc contents))))
    (dolist (tag tags)   (when tag (push tag (alist-get 'tag contents))))
    ;; (dolist (file files) (when file (push (concat "~/" (file-relative-name file "~/")) (alist-get 'file contents))))
    (dolist (url urls)   (when url (push url (alist-get 'url contents))))
    
    (dolist (elm reqs)
      (pcase elm
        (`(emacs ,ver)
         (let ((ver* (string-join (mapcar 'number-to-string ver) ".")))
           (push (format "emacs>=%s" ver*) (alist-get 'tag contents))
           (push (format "emacs-%s" ver*) (alist-get 'req contents))
           (push (string-to-number ver*) (alist-get 'emacs>= contents))))
        (`(,pkg ,ver)
         (let ((ver* (string-join (mapcar 'number-to-string ver) ".")))
           (push (format "%s-%s" pkg ver*) (alist-get 'req contents))
           (unless (memq pkg leaf-convert-except-after)
             (push pkg (alist-get 'after contents)))))))
    contents))

;;;###autoload
(defun leaf-convert-insert-template (pkg)
  "Insert template `leaf' block for PKG using package.el cache.
And kill generated leaf block to quick yank."
  (interactive
   ;; see `package-install'
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Package name: "
                    (thread-last (append
                                  (mapcar (lambda (elm) (symbol-name (car elm))) package-archive-contents)

                                  ;; see `load-library'
                                  (locate-file-completion-table load-path (get-load-suffixes) "" nil t))
                      (mapcar (lambda (elm) (if (string-suffix-p "/" elm) nil elm)))
                      (delete-dups)))))))
  (let ((standard-output (current-buffer)))
    (thread-last (leaf-convert-contents-new--sexp-internal `(prog1 ',pkg) nil 'toplevel)
      (leaf-convert--fill-info)
      (leaf-convert-from-contents)
      (leaf-pp)))
  (delete-char -1)
  (backward-sexp)
  (indent-sexp)
  (forward-sexp))


;;; Main

(defun leaf-convert--leaf-name-to-t (pkg key val)
  "Convert PKG symbol to t if KEY is the menber of omittable-keywords.
KEY and VAL is the key and value currently trying to convert.
CONTENTS is the value of all the leaf-convert-contents.

If VAL contains the same value as leaf--name, replace it with t."
  (pcase (list
          (memq key leaf-convert-leaf-name-omittable-keywords)
          (memq pkg val))
    (`(nil ,_) val)
    (`(,_ nil) val)
    (`(,_ ,_)  (append '(t) (delq pkg val)))))

(defun leaf-convert--expand-use-package (sexp)
  "Macroexpand-1 for use-package SEXP.

  (ppp-sexp
   (macroexpand-1
    '(use-package color-moccur
       :commands isearch-moccur
       :config
       (use-package moccur-edit))))
  ;;=> (progn
  ;;     (unless (fboundp 'isearch-moccur)
  ;;       (autoload #'isearch-moccur \"color-moccur\" nil t))
  ;;     (eval-after-load 'color-moccur
  ;;       '(progn
  ;;          (require 'moccur-edit nil nil)
  ;;          t)))
  
  (ppp-sexp
   (leaf-convert--expand-use-package
    '(use-package color-moccur
       :commands isearch-moccur
       :config
       (use-package moccur-edit))))
  ;;=> (progn
  ;;     (unless (fboundp 'isearch-moccur)
  ;;       (autoload #'isearch-moccur \"color-moccur\" nil t))
  ;;     (eval-after-load 'color-moccur
  ;;       '(progn
  ;;          (use-package moccur-edit)
  ;;          t)))"
  (let* ((op (car sexp))
         (sym (gensym))
         (form (thread-last sexp
                 (cl-subst sym 'use-package)
                 (funcall (lambda (elm) (setf (car elm) op) elm))
                 (funcall (lambda (elm)
                            (let ((use-package-expand-minimally t)
                                  (leaf-expand-minimally t))
                              (macroexpand-1 elm))))
                 (cl-subst 'use-package sym)))
         (form* (thread-last sexp
                  (cl-subst sym 'use-package)
                  (funcall (lambda (elm) (setf (car elm) op) elm))
                  (funcall (lambda (elm)
                             (let ((use-package-expand-minimally t)
                                   (leaf-expand-minimally t)
                                   (byte-compile-current-file t)  ; use-package hack
                                   (use-package-ensure-function 'ignore))
                               (macroexpand-1 elm))))
                  (cl-subst 'use-package sym)))
         extra)
    (dolist (elm form*)
      (pcase elm
        (`(eval-and-compile . ,forms)
         (dolist (e forms)
           (pcase e
             (`(defvar ,(and (pred symbolp) sym))
              (push `(defvar ,sym) extra)))))))
    (pcase form
      (`(progn . ,body)
       (if extra `(progn ,@(nreverse extra) ,@body) form))
      (_
       (if extra `(progn ,@(nreverse extra) ,form) form)))))

(defun leaf-convert-from-contents (contents)
  "Convert CONTENTS (as leaf-convert-contents) to leaf format."
  (unless leaf-keywords-init-frg
    (leaf-keywords-init))
  (let ((pkg (or (alist-get 'leaf-convert--name contents) 'leaf-convert))
        (all-keywords (leaf-available-keywords)))
    (when-let (unknown (thread-last (mapcar #'car contents)
                         (mapcar (lambda (key) (member key all-keywords)))
                         (delq nil)))
      (error "Unknown keyword%s included.  Unknown: %s"
             (if (= 1 (length unknown)) "" "s") unknown))
    `(leaf ,pkg
       ,@(mapcan
          (lambda (key)
            (let ((sym (intern (substring (symbol-name key) 1))))
              (when-let (value (thread-last (alist-get sym contents)
                                 (delete-dups)
                                 (nreverse)
                                 (leaf-convert--leaf-name-to-t pkg key)))
                (if (memq key leaf-convert-prefer-list-keywords)
                    `(,key ,value)
                  `(,key ,@value)))))
          all-keywords))))

;;;###autoload
(defalias 'leaf-convert 'leaf-convert-from-sexp)

;;;###autoload
(defmacro leaf-convert-from-sexp (&rest sexp)
  "Convert SEXP (as plain Elisp) to leaf format."
  `(leaf-convert-from-contents
    (leaf-convert-contents-new--sexp (progn ,@sexp))))

;;;###autoload
(defalias 'leaf-convert-from-leaf 'leaf-convert-from-use-package)

;;;###autoload
(defmacro leaf-convert-from-use-package (sexp)
  "Convert SEXP (as use-package) to leaf format."
  (pcase sexp
    (`(,(or 'use-package 'leaf) ,(and (pred symbolp) pkg) . ,_body)
     (let* ((form (leaf-convert--expand-use-package sexp))
            (target (pcase form
                      (`(progn . ,body) `(prog1 ',pkg ,@body))
                      (_ `(prog1 ',pkg ,form)))))
       `(leaf-convert-from-contents
         (leaf-convert-contents-new--sexp ,target))))
    (_
     (error "Toplevel sexp is not use-package or leaf.  sexp: %s" sexp))))

(provide 'leaf-convert)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert.el ends here
