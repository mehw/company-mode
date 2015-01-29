;;; regexp-tests.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: mehw

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Place regexp-tests.el in a directory and add the following to your .emacs:
;; (add-to-list 'load-path "/path/to/directory")
;; (require 'company-regexp-tests)

;; DEFINE HERE THE PREFIXES
(defvar company-regexp--test-prefixes
  '("foo"
    "bar"))

;; DEFINE HERE THE REGEXPS
(defconst company-regexp--test-regexps
  '("^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.+?\\)?\\)?$"
    "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.+\\)?\\)?$"
    "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.*\\)?\\)?$"))

;; DEFINE HERE THE TARGET STRINGS
(defvar company-regexp--test-strings
  '(
   ;; expected result: no capture (no prefix)
   "COMPLETION: "
   ;; expected result: no capture (wrong prefix)
   "COMPLETION: blabla"
   ;; expected result: group 1
   "COMPLETION: foobar"
   ;; expected result: group 1 (group 2, 3 are nil)
   "COMPLETION: foobar : "
   ;; expected result: group 1, 2 (group 3 is nil)
   "COMPLETION: foobar : [#int#]foobar(<#int a#>)"
   ;; expected result: group 1, 2 (group 3 is nil)
   "COMPLETION: foobar : [#int#]foobar(<#int a#>) : "
   ;; expected result: group 1, 2, 3
   "COMPLETION: foobar : [#int#]foobar(<#int a#>) : Comment 1"
   ;; expected result: group 1, 2, 3 ("Comment 1 : ")
   "COMPLETION: foobar : [#int#]foobar(<#int a#>) : Comment 1 : "))

(defun company-regexp-tests-run-tests nil
  (interactive)
  (let ((results-list) (pattern) (match) (hit))
    (push ";;; begin of document" results-list)
    (with-current-buffer (get-buffer-create "*regexp-testing-ground*")
      (dolist (t-prefix company-regexp--test-prefixes)
        (dolist (t-regexp company-regexp--test-regexps)
          (push (concat "prefix : \'" t-prefix "\'") results-list)
          (push (concat "regexp : \'" t-regexp "\'") results-list)
          (setq pattern (format t-regexp t-prefix))
          (push (concat " pattern: \'" pattern "\'") results-list)
          (dolist (t-string company-regexp--test-strings)
            (erase-buffer)
            (insert t-string)
            (goto-char (point-min))
            (push (concat "  string : \'" t-string "\'") results-list)
            (setq hit (re-search-forward pattern nil t))
            (dolist (group '(1 2 3))
              (if hit
                  (setq match (match-string-no-properties group))
                (setq match nil))
              (if match
                  (setq match (concat "\'" match "\'"))
                (setq match "nil"))
              (push (concat
                     "   group " (number-to-string group) ": " match)
                    results-list)))))
      (push ";;; end of document" results-list))
    (setq results-list (reverse results-list))
    (with-current-buffer (get-buffer-create "*regexp-testing-ground*")
      (erase-buffer)
      (dolist (line results-list)
        (insert line)
        (insert "\n")))))

(provide 'company-regexp-tests)
;;; regexp-tests.el ends here
