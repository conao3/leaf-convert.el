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
(require 'leaf)

(defgroup leaf-convert nil
  "Convert many format to leaf format."
  :prefix "leaf-convert-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-convert.el"))


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
  (when-let* ((pkg (alist-get 'leaf-convert--name contents))
              (desc (or
                     (if (package-desc-p pkg) pkg)
                     (cadr (assq pkg package-alist))
                     (let ((built-in (assq pkg package--builtins)))
                       (if built-in
                           (package--from-builtin built-in)
                         (cadr (assq pkg package-archive-contents)))))))
    (let* ((summary (package-desc-summary desc))
           (reqs (package-desc-reqs desc))
           (extras (package-desc-extras desc))
           (url (cdr (assoc :url extras)))
           (keywords (if desc (package-desc--keywords desc)))
           (path (locate-file (format "%s.el" pkg)
                              load-path
                              load-file-rep-suffixes)))
      (push summary (alist-get 'doc contents))
      (push path (alist-get 'file contents))
      (push url (alist-get 'url contents))
      (push keywords (alist-get 'tag contents))
      (dolist (elm reqs)
        (pcase elm
          (`(emacs ,ver)
           (push (format "emacs-%s" (string-join (mapcar 'number-to-string ver) "."))
                 (alist-get 'tag contents))))))
    contents))


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
