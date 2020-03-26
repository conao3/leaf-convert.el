;;; leaf-convert.el --- Convert many format to leaf format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1") (leaf "3.6.0"))
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

(defgroup leaf-convert nil
  "Convert many format to leaf format."
  :prefix "leaf-convert-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-convert.el"))

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
(defun leaf-convert--string-or-symbol (elm default)
  "Convert ELM to symbol.  If ELM is nil, return DEFAULT.
ELM can be string or symbol."
  (or (if (stringp elm) (intern elm) elm)
      default))

(defun leaf-convert-contents-new--sexp-1 (sexp contents)
  "Internal recursive function of `leaf-convert-contents-new--sexp'.
Add convert SEXP to leaf-convert-contents to CONTENTS."
  (pcase sexp
    ;; :load-path, :load-path*
    (`(add-to-list 'load-path ,(and (pred stringp) elm))
     (push elm (alist-get 'load-path contents)))
    (`(add-to-list 'load-path (locate-user-emacs-file ,(and (pred stringp) elm)))
     (push elm (alist-get 'load-path* contents)))
    (`(add-to-list 'load-path (concat user-emacs-directory ,(and (pred stringp) elm)))
     (push elm (alist-get 'load-path* contents)))

    ;; :defun
    (`(declare-function ,elm ,(and (pred stringp) file) . ,_args)
     (push `(,elm . ,(intern file)) (alist-get 'defun contents)))

    ;; :defvar
    (`(defvar ,elm)
     (push elm (alist-get 'defvar contents)))
    (`(defvar ,elm ,val)
     (push `(,elm . ,val) (alist-get 'setq contents)))

    ;; any
    (_ (push sexp (alist-get 'config contents))))
  contents)

(defun leaf-convert-contents-new--sexp-internal (sexp &optional contents toplevel)
  "Internal function of `leaf-convert-contents-new--sexp'.
Convert SEXP to leaf-convert-contents.
If specified CONTENTS, add value to it instead of create new instance.
When TOPLEVEL is non-nil, it converts sexp, which affects the
whole block like `eval-after-load', into leaf keyword.'"
  (pcase sexp
    (`(progn . ,body)
     (dolist (elm body)
       (setq contents
             (leaf-convert-contents-new--sexp-internal
              elm contents (and toplevel (equal elm (car body)))))))
    (`(prog1 ,(or `(quote ,name)
                  (and (pred stringp) name))
        . ,body)
     (setf (alist-get 'leaf-convert--name contents) name)
     (dolist (elm body)
       (setq contents
             (leaf-convert-contents-new--sexp-internal
              elm contents (and toplevel (equal elm (car body)))))))
    (`(with-eval-after-load ,(or `(quote ,name)
                                 (and (pred stringp) name))
        . ,body)
     (setq contents
           (leaf-convert-contents-new--sexp-internal
            `(eval-after-load ',name '(progn ,@body)) contents toplevel)))
    (`(eval-after-load ,(or `(quote ,name)
                            (and (pred stringp) name))
        (quote ,body))
     (if (not toplevel)
         (setq contents
               (leaf-convert-contents-new--sexp-1 body contents))
       (setf (alist-get 'leaf-convert--name contents) name)
       (push name (alist-get 'after contents))
       (setq contents
             (leaf-convert-contents-new--sexp-internal
              body contents toplevel))))
    (_
     (setq contents
           (leaf-convert-contents-new--sexp-1 sexp contents))))
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
                    "Install package: "
                    (delete-dups
                     (mapcar
                      (lambda (elm) (if (string-suffix-p "/" elm) nil elm))
                      (append
                       (mapcar (lambda (elm) (symbol-name (car elm))) package-archive-contents)

                       ;; see `load-library'
                       (locate-file-completion-table load-path (get-load-suffixes) "" nil t)))))))))
  (let ((standard-output (current-buffer)))
    (leaf-pp
     (leaf-convert-from-contents
      (leaf-convert--fill-info
       (leaf-convert-contents-new--sexp-internal
        `(prog1 ',pkg) nil 'toplevel)))))
  (delete-char -1)
  (backward-sexp)
  (indent-sexp)
  (forward-sexp))


;;; Main

(defun leaf-convert-from-contents (contents)
  "Convert CONTENTS (as leaf-convert-contents) to leaf format."
  `(leaf ,(leaf-convert--string-or-symbol
           (alist-get 'leaf-convert--name contents)
           'leaf-convert)
     ,@(mapcan (lambda (keyword)
                 (let ((key (intern (substring (symbol-name keyword) 1))))
                   (when-let (value (alist-get key contents))
                     `(,keyword ,@(nreverse value)))))
               (leaf-available-keywords))))

;;;###autoload
(defalias 'leaf-convert 'leaf-convert-from-sexp)

;;;###autoload
(defmacro leaf-convert-from-sexp (&rest sexp)
  "Convert SEXP (as plain Elisp) to leaf format."
  `(leaf-convert-from-contents
    (leaf-convert-contents-new--sexp (progn ,@sexp))))

(provide 'leaf-convert)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert.el ends here
