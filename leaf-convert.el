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

(eval-and-compile
  (defvar leaf-convert-slots
    '(leaf-convert--name            ; leaf-convert only internal keyword
      disabled leaf-protect
      load-path load-path*
      leaf-autoload
      doc tag file url
      defun defvar
      leaf-defun leaf-defvar
      preface
      when unless if
      ensure package
      straight el-get
      after commands
      bind bind*
      mode interpreter
      magic magic-fallback
      hook
      advice advice-remove
      init pre-setq
      pl-pre-setq auth-pre-setq
      custom pl-custom auth-custom
      custom-face
      hydra combo combo*
      smartrep smartrep*
      chord chord*
      leaf-defer
      require
      config
      diminish delight
      setq setq-default
      pl-setq auth-setq
      pl-setq-default auth-setq-default)
    "Leaf slots.

This list can be created by below sexp.

 (mapcar (lambda (elm)
            (intern (substring (symbol-name elm) 1)))
          (leaf-available-keywords))")

  (eval
   `(cl-defstruct (leaf-convert-contents
                   (:constructor nil)
                   (:constructor leaf-convert-contents-new
                                 (&key ,@leaf-convert-slots))
                   (:copier nil))
      "Contents of leaf.
If specify nil as value, use :leaf-convert--nil instead of just nil."
      ,@leaf-convert-slots)))


;;; Functions
(defun leaf-convert--string-or-symbol (elm default)
  "Convert ELM to symbol.  If ELM is nil, return DEFAULT.
ELM can be string or symbol."
  (or (if (stringp elm) (intern elm) elm)
      default))

(defmacro leaf-convert--setf-or-push (elm place)
  "If PLACE is nil, just setf ELM, if PLACE is non-nil, push ELM."
  `(if ,place
       (if (and (listp ,place)
                (not (leaf-pairp ,place)))
           (push ,elm ,place)
         (setf ,place (list ,elm ,place)))
     (setf ,place ,elm)))

(defun leaf-convert-contents-new--sexp-1 (sexp contents)
  "Internal recursive function of `leaf-convert-contents-new--sexp'.
Add convert SEXP to leaf-convert-contents to CONTENTS."
  (pcase sexp
    ;; :load-path, :load-path*
    (`(add-to-list 'load-path ,(and (pred stringp) elm))
     (leaf-convert--setf-or-push elm (leaf-convert-contents-load-path contents)))
    (`(add-to-list 'load-path (locate-user-emacs-file ,(and (pred stringp) elm)))
     (leaf-convert--setf-or-push elm (leaf-convert-contents-load-path* contents)))
    (`(add-to-list 'load-path (concat user-emacs-directory ,(and (pred stringp) elm)))
     (leaf-convert--setf-or-push elm (leaf-convert-contents-load-path* contents)))

    ;; :defun
    (`(declare-function ,elm)
     (leaf-convert--setf-or-push elm (leaf-convert-contents-defun contents)))
    (`(declare-function ,elm ,(and (pred stringp) file))
     (leaf-convert--setf-or-push `(,elm . ,(intern file)) (leaf-convert-contents-defun contents)))
    (`(declare-function ,elm ,(and (pred stringp) file) ,_args)
     (leaf-convert--setf-or-push `(,elm . ,(intern file)) (leaf-convert-contents-defun contents)))

    ;; :defvar
    (`(defvar ,elm)
     (leaf-convert--setf-or-push elm (leaf-convert-contents-defvar contents)))

    ;; any
    (_ (push sexp (leaf-convert-contents-config contents))))
  contents)

(defmacro leaf-convert-contents-new--sexp (sexp &optional contents)
  "Convert SEXP to leaf-convert-contents.
If specified CONTENTS, add value to it instead of new instance."
  (let ((contents* (or (eval contents) (leaf-convert-contents-new))))
    (pcase sexp
      (`(progn . ,body)
       (dolist (elm body)
         (setq contents*
               (leaf-convert-contents-new--sexp-1 elm contents*))))
      (`(prog1 ,(or `(quote ,name)
                    (and (pred stringp) name))
          . ,body)
       (setf (leaf-convert-contents-leaf-convert--name contents*) name)
       (dolist (elm body)
         (setq contents*
               (leaf-convert-contents-new--sexp-1 elm contents*))))
      (`(with-eval-after-load ,(or `(quote ,name)
                                   (and (pred stringp) name))
          . ,body)
       (setf (leaf-convert-contents-leaf-convert--name contents*) name)
       (setf (leaf-convert-contents-after contents*) t)
       (dolist (elm body)
         (setq contents*
               (leaf-convert-contents-new--sexp-1 elm contents*))))
      (_
       (leaf-convert-contents-new--sexp-1 sexp contents*)))
    contents*))

(defun leaf-convert--fill-info (contents)
  "Add :doc, :file, :url information to CONTENTS."
  ;; see `describe-package-1'
  (when-let* ((pkg (leaf-convert-contents-leaf-convert--name contents))
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
      (leaf-convert--setf-or-push summary
        (leaf-convert-contents-doc contents))
      (leaf-convert--setf-or-push path
        (leaf-convert-contents-file contents))
      (leaf-convert--setf-or-push url
        (leaf-convert-contents-url contents))
      (leaf-convert--setf-or-push keywords
        (leaf-convert-contents-tag contents))
      (dolist (elm reqs)
        (pcase elm
          (`(emacs ,ver)
           (leaf-convert--setf-or-push
               (format "emacs-%s"
                       (string-join (mapcar 'number-to-string ver) "."))
             (leaf-convert-contents-tag contents))))))
    contents))


;;; Main

(defun leaf-convert-from-contents (contents)
  "Convert CONTENTS (as leaf-convert-contents) to leaf format."
  (if (not (leaf-convert-contents-p contents))
      (error "CONTENTS must be a instance of leaf-convert-contents")
    `(leaf ,(leaf-convert--string-or-symbol
             (leaf-convert-contents-leaf-convert--name contents)
             'leaf-convert)
       ,@(mapcan (lambda (elm)
                   (let ((fn (intern
                              (format "leaf-convert-contents-%s"
                                      (substring (symbol-name elm) 1)))))
                     (when (fboundp fn)
                       (when-let (value (funcall fn contents))
                         `(,elm
                           ,@(cond
                              ((memq elm '(:preface :init :config))
                               value)
                              (t
                               (if (eq value :leaf-convert--nil) '(nil) `(,value)))))))))
                 (leaf-available-keywords)))))

;;;###autoload
(defalias 'leaf-convert 'leaf-convert-from-sexp)

;;;###autoload
(defmacro leaf-convert-from-sexp (sexp)
  "Convert SEXP (as plain Elisp) to leaf format."
  `(leaf-convert-from-contents
    (leaf-convert-contents-new--sexp ,sexp)))

(provide 'leaf-convert)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert.el ends here
