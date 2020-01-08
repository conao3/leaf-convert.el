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
     (leaf-convert-from-contents
      (leaf-convert-contents-new))
     :to-equal
     '(leaf leaf-convert))

    (expect
     (leaf-convert-from-contents
      (leaf-convert-contents-new
       :name "some-package"))
     :to-equal
     '(leaf some-package))

    (expect
     (leaf-convert-from-contents
      (leaf-convert-contents-new
       :name 'some-package))
     :to-equal
     '(leaf some-package)))

  (it "Disabled"
    (expect
     (leaf-convert-from-contents
      (leaf-convert-contents-new
       :disabled t))
     :to-equal
     '(leaf leaf-convert
        :disabled t))

    ;; (expect
    ;;  (leaf-convert-from-contents
    ;;   (leaf-convert-contents-new
    ;;    :disabled nil))
    ;;  :to-equal
    ;;  '(leaf leaf-convert))
    ))

;; (provide 'leaf-convert-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf-convert-test.el ends here
