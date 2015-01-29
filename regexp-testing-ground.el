;;; regexp-testing-ground.el

;; DEFINE HERE THE PREFIXES
(defvar test-prefixes
  '("foo"
    "bar"))

;; DEFINE HERE THE REGEXPS
(defconst test-regexps
  '("^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.+?\\)?\\)?$"
    "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.+\\)?\\)?$"
    "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.*\\)?\\)?$"))

;; DEFINE HERE THE TARGET STRINGS
(defvar test-strings
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

(defun regexp-testing-ground-run-tests nil
  (interactive)
  (let ((results-list) (pattern) (match) (hit))
    (push ";;; begin of document" results-list)
    (with-current-buffer (get-buffer-create "*regexp-testing-ground*")
      (dolist (t-prefix test-prefixes)
        (dolist (t-regexp test-regexps)
          (push (concat "prefix : \'" t-prefix "\'") results-list)
          (push (concat "regexp : \'" t-regexp "\'") results-list)
          (setq pattern (format t-regexp t-prefix))
          (push (concat " pattern: \'" pattern "\'") results-list)
          (dolist (t-string test-strings)
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

(provide 'regexp-testing-ground)
;;; regexp-testing-ground.el ends here
