;;; leaf-convert.el --- Convert many format to leaf format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1"))
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

;; Convert many format to leaf format.


;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup leaf-convert nil
  "Convert many format to leaf format."
  :prefix "leaf-convert-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/leaf-convert.el"))

(eval-and-compile
  (defvar leaf-convert-slots
    '(name
      disabled leaf-protect load-path
      leaf-autoload
      doc file url
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

(defun leaf-convert-contents-new--from-sexp (sexp &optional contents)
  "Convert SEXP to leaf-convert-contents.
If specified CONTENTS, add value to it instead of new instance."
  (let ((contents* (or contents (leaf-convert-contents-new))))
    (pcase sexp
      (`(add-to-list 'load-path ,(and (pred stringp) elm))
       (push elm (leaf-convert-contents-load-path contents*)))
      (_ (push sexp (leaf-convert-contents-config contents*))))
    contents*))

(defun leaf-convert--fill-info (contents)
  "Add :doc, :file, :url information to CONTENTS."
  ;; see `describe-package-1'
  (when-let* ((pkg (leaf-convert-contents-name contents))
              (desc (or
                     (if (package-desc-p pkg) pkg)
                     (cadr (assq pkg package-alist))
                     (let ((built-in (assq pkg package--builtins)))
                       (if built-in
                           (package--from-builtin built-in)
                         (cadr (assq pkg package-archive-contents)))))))
    (let* ((summary (package-desc-summary desc))
           (_reqs (package-desc-reqs desc))
           (extras (package-desc-extras desc))
           (url (cdr (assoc :url extras)))
           (_keywords (if desc (package-desc--keywords desc)))
           (path (locate-file (format "%s.el" pkg)
                              load-path
                              load-file-rep-suffixes)))
      (leaf-convert--setf-or-push summary
        (leaf-convert-contents-doc contents))
      (leaf-convert--setf-or-push path
        (leaf-convert-contents-file contents))
      (leaf-convert--setf-or-push url
        (leaf-convert-contents-url contents))
      ;; (setf (leaf-convert-contents-tag contents) keywords)
      )
    contents))


;;; Main

;;;###autoload
(defun leaf-convert-from-contents (contents)
  "Convert CONTENTS to leaf format using LEAF-NAME."
  (if (not (leaf-convert-contents-p contents))
      (error "CONTENTS must be a instance of leaf-convert-contents")
    `(leaf ,(leaf-convert--string-or-symbol
             (leaf-convert-contents-name contents)
             'leaf-convert)
       ,@(mapcan (lambda (elm)
                   (let ((fn (intern
                              (format "leaf-convert-contents-%s" elm))))
                     (when-let (value (funcall fn contents))
                       `(,(intern (format ":%s" elm))
                         ,(if (eq value :leaf-convert--nil) nil value)))))
                 (remq 'name leaf-convert-slots)))))

(provide 'leaf-convert)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert.el ends here
