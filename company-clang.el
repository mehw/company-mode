;;; company-clang.el --- company-mode completion back-end for Clang  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2014  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-template)
(require 'cl-lib)

(defgroup company-clang nil
  "Completion back-end for Clang."
  :group 'company)

(defcustom company-clang-executable
  (executable-find "clang")
  "Location of clang executable."
  :type 'file)

(defcustom company-clang-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\".")

(defcustom company-clang-arguments nil
  "Additional arguments to pass to clang when completing.
Prefix files (-include ...) can be selected with `company-clang-set-prefix'
or automatically through a custom `company-clang-prefix-guesser'."
  :type '(repeat (string :tag "Argument")))

(defcustom company-clang-prefix-guesser 'company-clang-guess-prefix
  "A function to determine the prefix file for the current buffer."
  :type '(function :tag "Guesser function" nil))

(defvar company-clang-modes '(c-mode c++-mode objc-mode)
  "Major modes which clang may complete.")

(defcustom company-clang-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.0"))

;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-clang--prefix nil)

(defsubst company-clang--guess-pch-file (file)
  (let ((dir (directory-file-name (file-name-directory file))))
    (when (equal (file-name-nondirectory dir) "Classes")
      (setq dir (file-name-directory dir)))
    (car (directory-files dir t "\\([^.]h\\|[^h]\\).pch\\'" t))))

(defsubst company-clang--file-substring (file beg end)
  (with-temp-buffer
    (insert-file-contents-literally file nil beg end)
    (buffer-string)))

(defconst company-clang-parse-comments-min-version 3.2
  "Starting from version 3.2 Clang can parse comments.")

(defcustom company-clang-temporary-file (quote ("clang-output" nil))
  "Experimental (tested on GNU/Linux).

Capture Clang's output trough a temporary file (could be faster
than capturing the output directly into Emacs."
  :type '(radio
          (const :tag "Disable" nil)
          (group
           (string :tag "Filename" "clang-output")
           (checklist
            (const :tag "Debug (leave temporary file in temporary directory)" t)))))

(defcustom company-clang-parse-comments '(t t t)
  "Parse completions' documentation comments.

Requires Clang version 3.2 or above."
  :type '(choice
          (const :tag "No" nil)
          (group :tag "Yes"
                 (const :tag "Yes" t) ;; new line trick
                 (boolean :tag "   Include system headers" t)
                 (boolean :tag "   Parse comments only on demand" t))))

(defcustom company-clang-documentation-fill-column 70
  "Column beyond which automatic line-wrapping should happen."
  :type 'integer)

(defcustom company-clang-documentation-justify 'full
  "Specifies which kind of justification to do."
  :type '(choice (const :tag "Full" full)
                 (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Center" center)
                 (const :tag "None" nil)))

(defvar company-clang--doc-list nil
  "Association list of tag's index + documentation.")

(defconst company-clang--AST-head
  "^Dumping %s:$")

(defconst company-clang--AST-Decl
  "^.* %s '\\(.*\\)'$")

(defconst company-clang--AST-TextComment
  "TextComment.*Text=\" \\(.*\\) \"$")

(defconst company-clang--meta-no-prefix
  "\\(%s\\).*\\'")

(defconst company-clang--meta-no-args
  "\\( [a-zA-Z0-9_:]+\\)\\(?:,\\|)\\)")

(defvar company-clang-set-ast-doc-hook nil
  "Hooks to call after `company-clang--set-ast-doc' has been run
with a positive result.")

(defun company-clang--strip-meta (candidate)
  "Retrun CANDIDATE's meta stripped from prefix and args."
  (let* ((prefix (regexp-quote candidate))
         (meta (company-clang--meta candidate))
         (strip-prefix
          (format company-clang--meta-no-prefix prefix))
         (strip-args company-clang--meta-no-args))
    (replace-regexp-in-string
     strip-args ""
     (replace-regexp-in-string
      strip-prefix "" meta nil nil 1) nil nil 1)))

(defun company-clang--parse-AST (candidate)
  "Return the CANDIDATE's AST.

Resolve function overloads."
  (goto-char (point-min))
  (let* ((prefix (regexp-quote candidate))
         (meta (company-clang--strip-meta candidate))
         (head (format company-clang--AST-head prefix))
         (decl (format company-clang--AST-Decl prefix))
         (abort nil)
         proto head-beg head-end empty-line)
    (while (not abort)
      (if (not (re-search-forward head nil t))
          (setq abort t)
        (setq head-beg (match-beginning 0))
        (setq head-end (match-end 0))
        (if (not (re-search-forward "^$" nil t))
            (setq abort t)
          (setq empty-line (match-end 0))
          (goto-char (+ head-end 1))
          (when (re-search-forward decl empty-line t)
            (setq proto (match-string-no-properties 1))
            (when (string= proto meta)
              (setq abort 'ok))))))
    (when (eq abort 'ok)
      (buffer-substring head-beg empty-line))))

(defun company-clang--can-parse-comments nil
  "Verify that the version of Clang in use can parse comments."
  (>= company-clang--version
      company-clang-parse-comments-min-version))

(defun company-clang--set-candidate-index (candidate)
  "Set the index of a CANDIDATE and return the index."
  (let ((index (cl-gensym)))
    (put-text-property 0 1 'cc-tag index candidate)
    index))

(defun company-clang--get-candidate-index (candidate)
  "Extract the index of a CANDIDATE."
  (get-text-property 0 'cc-tag candidate))

(defun company-clang--set-candidate-doc (doc candidate)
  "Set the documentation of a CANDIDATE and return its index.

Prevent duplicated records."
  (let* ((index (company-clang--get-candidate-index candidate))
         (record (assoc index company-clang--doc-list)))
    (if index
        (when record
          (setq company-clang--doc-list
                (delq record company-clang--doc-list)))
      (setq index (company-clang--set-candidate-index candidate)))
    (push (list index doc) company-clang--doc-list)
    index))

(defun company-clang--get-candidate-doc (candidate)
  "Extract the documentation of a CANDIDATE."
  (let* ((index (company-clang--get-candidate-index candidate))
         (record (assoc index company-clang--doc-list)))
    (unless record
      (add-hook 'company-clang-set-ast-doc-hook 'company-clang--doc-buffer)
      (company-clang--AST-process candidate 'company-clang--set-ast-doc))
    (car (cdr record))))

(defun company-clang--set-ast-doc (candidate ast)
  "Set the documentation of a CANDIDATE reading it from its AST."
  (let (doc)
    (when (stringp ast)
      (with-temp-buffer
        (insert ast)
        (goto-char (point-min))
        (re-search-forward company-clang--AST-TextComment nil t)
        (setq doc (match-string-no-properties 1))))
    (company-clang--set-candidate-doc doc candidate)
    (when doc
      (run-hook-with-args 'company-clang-set-ast-doc-hook candidate))))

(defun company-clang--doc-buffer (candidate)
  "Create the documentation buffer for a CANDIDATE."
  ;; FIXME: should the hook have an auto-remove feature?
  (remove-hook 'company-clang-set-ast-doc-hook 'company-clang--doc-buffer)
  (let ((meta (company-clang--meta candidate))
        (doc (company-clang--get-candidate-doc candidate))
        (emptylines "\n\n"))
    (unless (and doc meta)
      (setq emptylines ""))
    (when (or doc meta)
      (company-doc-buffer
       (concat meta
               emptylines
               (company-clang-string-to-paragraph
                doc
                company-clang-documentation-fill-column
                company-clang-documentation-justify))))))

(defun company-clang-string-to-paragraph (str &optional len justify)
  "Convert STR to a paragraph.

LEN controls the width.

JUSTIFY specifies which kind of justification to do: `full',
`left', `right', `center', or `none' (equivalent to nil).  A
value of t means handle each paragraph as specified by its text
properties."
  (when str
    (if (or (eq justify 'full)
            (eq justify 'left)
            (eq justify 'right)
            (eq justify 'center))
        (with-temp-buffer
          (insert str)
          (when len
            (setq fill-column len))
          (fill-region (point-min) (point-max) justify)
          (buffer-string))
      str)))

(defun company-clang-guess-prefix ()
  "Try to guess the prefix file for the current buffer."
  ;; Prefixes seem to be called .pch.  Pre-compiled headers do, too.
  ;; So we look at the magic number to rule them out.
  (let* ((file (company-clang--guess-pch-file buffer-file-name))
         (magic-number (and file (company-clang--file-substring file 0 4))))
    (unless (member magic-number '("CPCH" "gpch"))
      file)))

(defun company-clang-set-prefix (&optional prefix)
  "Use PREFIX as a prefix (-include ...) file for clang completion."
  (interactive (let ((def (funcall company-clang-prefix-guesser)))
     (unless (stringp def)
       (setq def default-directory))
     (list (read-file-name "Prefix file: "
                           (when def (file-name-directory def))
                           def t (when def (file-name-nondirectory def))))))
  ;; TODO: pre-compile?
  (setq company-clang--prefix (and (stringp prefix)
                                   (file-regular-p prefix)
                                   prefix)))

;; Clean-up on exit.
(add-hook 'kill-emacs-hook 'company-clang-set-prefix)

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Handle Pattern (syntactic hints would be neat).
;; Do we ever see OVERLOAD (or OVERRIDE)?
(defconst company-clang--completion-pattern
  "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.+?\\)?\\)?\\(?: : \\(.+?\\)?\\)?$")

(defconst company-clang--error-buffer-name "*clang-error*")

(defun company-clang--lang-option ()
     (if (eq major-mode 'objc-mode)
         (if (string= "m" (file-name-extension buffer-file-name))
             "objective-c" "objective-c++")
       (substring (symbol-name major-mode) 0 -5)))

(defun company-clang--parse-output (prefix _objc)
  (goto-char (point-min))
  (let ((pattern (format company-clang--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        lines match)
    ;; BUGTESTING (time measurement)
    ;; ----------
    (when (eq parse-time-start nil)
      (setq parse-time-start (current-time-pico))
      (message "parse-start: %s" parse-time-start))
    ;; ----------
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (equal match "Pattern")
        (save-match-data
          (when (string-match ":" match)
            (setq match (substring match 0 (match-beginning 0)))))
        (let ((meta (match-string-no-properties 2))
              (doc (match-string-no-properties 3)))
          (when (and meta (not (string= match meta)))
            (put-text-property 0 1 'meta
                               (company-clang--strip-formatting meta)
                               match))
          (when doc
            (company-clang--set-candidate-doc doc match)))
        (push match lines)))
    ;; BUGTESTING (time measurement)
    ;; ----------
    (setq parse-time-stop (current-time-pico))
    (let ((delta (- parse-time-stop parse-time-start)))
      (message "parse-end: %s" (current-time-pico))
      (message "parse-delta: %s" (time-pico-to-seconds delta)))
    (setq parse-time-start nil)
    ;; ----------
    lines))

(defun company-clang--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-clang--annotation (candidate)
  (let ((meta (company-clang--meta candidate)))
    (cond
     ((null meta) nil)
     ((string-match "[^:]:[^:]" meta)
      (substring meta (1+ (match-beginning 0))))
     ((string-match "\\((.*)[ a-z]*\\'\\)" meta)
      (let ((paren (match-beginning 1)))
        (if (not (eq (aref meta (1- paren)) ?>))
            (match-string 1 meta)
          (with-temp-buffer
            (insert meta)
            (goto-char paren)
            (substring meta (1- (search-backward "<"))))))))))

(defun company-clang--strip-formatting (text)
  (replace-regexp-in-string
   "#]" " "
   (replace-regexp-in-string "[<{[]#\\|#[>}]" "" text t)
   t))

(defun company-clang--handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create company-clang--error-buffer-name))
         (cmd (concat company-clang-executable " " (mapconcat 'identity args " ")))
         (pattern (format company-clang--completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more aggressively if no match was found.
                (message "clang failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nclang failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun company-clang--redirect-output (buf args)
  "Redirect Clang's output to `company-clang-temporary-file'
instead than piping it directly to BUF when required."
  (let ((tmp-file (concat temporary-file-directory
                          (car company-clang-temporary-file))))
    ;; If `tmp-file' isn't a file, `company-clang-temporary-file' is
    ;; nil or doesn't have a file name set.
    (if (and (not (file-directory-p tmp-file))
             (with-temp-file tmp-file t))
        (progn
          (setq args (append args (list "1>" tmp-file)))
          (list tmp-file
                (lambda ()
                  (apply #'start-process-shell-command
                         "company-clang" nil company-clang-executable args))))
      (progn
        (when company-clang-temporary-file
          (message "Cannot create temporary file: falling back on stdout."))
        (list nil
              (lambda ()
                (apply #'start-process
                       "company-clang" buf company-clang-executable args)))))))

(defun company-clang--AST-process (candidate callback)
  "Process the CANDIDATE's AST.

Call CALLBACK passing CANDIDATE and its AST as arguments."
  ;; NOTE: build the args while in the original buffer.
  (let* ((prefix (regexp-quote candidate))
         (args (company-clang--build-AST-args prefix))
         (buf (get-buffer-create "*clang-output*"))
         (process-adaptive-read-buffering nil))
    (unless (get-buffer-process buf)
      (let* ((cmd (company-clang--redirect-output buf args))
             (tmp-file (car cmd))
             (process (car (cdr cmd)))
             (tmp-debug (car (car (cdr company-clang-temporary-file)))))
        ;; BUGTESTING (time measurement)
        ;; ----------
        (when (eq ast-time-start nil)
          (setq ast-time-start (current-time-pico))
          (message "ast-start: %s" ast-time-start))
        ;; ----------
        (setq process (funcall process))
        (set-process-sentinel
         process
         (lambda (proc status)
           (unless (string-match-p "hangup" status)
             (funcall
              callback
              candidate
              (let ((res (process-exit-status proc)))
                (with-current-buffer buf
                  ;; If `tmp-file' is a string, due to previous
                  ;; processing, it is a legit file.
                  (when (stringp tmp-file)
                    (if (file-readable-p tmp-file)
                        (progn
                          (erase-buffer)
                          (insert-file-contents-literally tmp-file)
                          (unless tmp-debug
                            (delete-file tmp-file)))
                      (message "Cannot read temporary file.")))
                  ;; FIXME: `company-clang--handle-error' seems to
                  ;; create troubles some time, we should suppress
                  ;; Clang's errors, in the meantime do not consider
                  ;; the return code 1 as an error.
                  (unless (or (eq 0 res) (eq 1 res))
                    (company-clang--handle-error res args))
                  ;; BUGTESTING (time measurement)
                  ;; ----------
                  (setq ast-time-stop (current-time-pico))
                  (let ((delta (- ast-time-stop ast-time-start)))
                    (message "ast-end: %s" (current-time-pico))
                    (message "ast-delta: %s" (time-pico-to-seconds delta)))
                  (setq ast-time-start nil)
                  ;; ----------
                  (company-clang--parse-AST candidate)))))))
        (unless (company-clang--auto-save-p)
          (send-region process (point-min) (point-max))
          (send-string process "\n")
          (process-send-eof process))))))

(defun company-clang--start-process (prefix callback &rest args)
  (setq company-clang--doc-list nil)
  (let ((objc (derived-mode-p 'objc-mode))
        (buf (get-buffer-create "*clang-output*"))
        (process-adaptive-read-buffering nil))
    (with-current-buffer buf (erase-buffer))
    (if (get-buffer-process buf)
        (funcall callback nil)
      (let* ((cmd (company-clang--redirect-output buf args))
             (tmp-file (car cmd))
             (process (car (cdr cmd)))
             (tmp-debug (car (car (cdr company-clang-temporary-file)))))
        ;; BUGTESTING (time measurement)
        ;; ----------
        (when (eq process-time-start nil)
          (setq process-time-start (current-time-pico))
          (message "process-start: %s" process-time-start))
        ;; ----------
        (setq process (funcall process))
        (set-process-sentinel
         process
         (lambda (proc status)
           (unless (string-match-p "hangup" status)
             (funcall
              callback
              (let ((res (process-exit-status proc)))
                (with-current-buffer buf
                  ;; If `tmp-file' is a string, due to previous
                  ;; processing, it is a legit file.
                  (when (stringp tmp-file)
                    (if (file-readable-p tmp-file)
                        (progn
                          (erase-buffer)
                          (insert-file-contents-literally tmp-file)
                          (unless tmp-debug
                            (delete-file tmp-file)))
                      (message "Cannot read temporary file.")))
                  (unless (eq 0 res)
                    (company-clang--handle-error res args))
                  ;; BUGTESTING (time measurement)
                  ;; ----------
                  (setq process-time-stop (current-time-pico))
                  (let ((delta (- process-time-stop process-time-start)))
                    (message "process-end: %s" (current-time-pico))
                    (message "process-delta: %s" (time-pico-to-seconds delta)))
                  (setq process-time-start nil)
                  ;; ----------
                  ;; Still try to get any useful input.
                  (company-clang--parse-output prefix objc)))))))
        (unless (company-clang--auto-save-p)
          (send-region process (point-min) (point-max))
          (send-string process "\n")
          (process-send-eof process))))))

(defsubst company-clang--build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d"
            (if (company-clang--auto-save-p) buffer-file-name "-")
            (line-number-at-pos)
            (1+ (length
                 (encode-coding-region
                  (line-beginning-position)
                  (point)
                  'utf-8
                  t))))))

(defun company-clang--expand-args (args)
  "Get the full Clang's front-end command line instruction."
  (with-temp-buffer
    (call-process-shell-command
     (mapconcat 'identity
                (append
                 (list "echo |")
                 (list company-clang-executable)
                 (list "-v") args) " ") nil t nil)
    (let (beg end)
      (goto-char (point-min))
      (when (re-search-forward "^ \".*\"" nil t)
        (setq beg (+ (match-end 0) 1))
        (when (re-search-forward "^clang.*version.*$" nil t)
          (setq end (- (match-beginning 0) 1))
          (split-string (buffer-substring beg end) " "))))))

(defun company-clang--parse-comments-args nil
  "Return the Clang's args needed to parse comments."
  (when (and company-clang-parse-comments
             (company-clang--can-parse-comments))
    (let ((parse-system (nth 1 company-clang-parse-comments))
          (on-demand (nth 2 company-clang-parse-comments)))
      (append
       (when parse-system
         (list "-Xclang" "--no-system-header-prefix="))
       (unless on-demand
         (list "-Xclang" "-code-completion-brief-comments"))))))

(defun company-clang--build-AST-args (prefix)
  "Return Clang's args to dump the AST filtering by PREFIX"
  (delete
   "-fcolor-diagnostics"
   (company-clang--expand-args
    (append '("-fsyntax-only" "-w" "-Xclang" "-ast-dump"
              "-Xclang" "-ast-dump-filter" "-Xclang")
            (list prefix)
            (company-clang--parse-comments-args)
            (unless (company-clang--auto-save-p)
              (list "-x" (company-clang--lang-option)))
            company-clang-arguments
            (when (stringp company-clang--prefix)
              (list "-include" (expand-file-name company-clang--prefix)))
            (list (if (company-clang--auto-save-p) buffer-file-name "-"))))))

(defsubst company-clang--build-complete-args (pos)
  (append '("-fsyntax-only" "-Xclang" "-code-completion-macros")
          (company-clang--parse-comments-args)
          (unless (company-clang--auto-save-p)
            (list "-x" (company-clang--lang-option)))
          company-clang-arguments
          (when (stringp company-clang--prefix)
            (list "-include" (expand-file-name company-clang--prefix)))
          (list "-Xclang" (format "-code-completion-at=%s"
                                  (company-clang--build-location pos)))
          (list (if (company-clang--auto-save-p) buffer-file-name "-"))))

(defun company-clang--candidates (prefix callback)
  (and (company-clang--auto-save-p)
       (buffer-modified-p)
       (basic-save-buffer))
  (when (null company-clang--prefix)
    (company-clang-set-prefix (or (funcall company-clang-prefix-guesser)
                                  'none)))
  (apply 'company-clang--start-process
         prefix
         callback
         (company-clang--build-complete-args (- (point) (length prefix)))))

(defun company-clang--prefix ()
  (if company-clang-begin-after-member-access
      (company-grab-symbol-cons "\\.\\|->\\|::" 2)
    (company-grab-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-clang-required-version 1.1)

(defvar company-clang--version nil)

(defun company-clang--auto-save-p ()
  (< company-clang--version 2.9))

(defsubst company-clang-version ()
  "Return the version of `company-clang-executable'."
  (with-temp-buffer
    (call-process company-clang-executable nil t nil "--version")
    (goto-char (point-min))
    (if (re-search-forward "clang\\(?: version \\|-\\)\\([0-9.]+\\)" nil t)
        (let ((ver (string-to-number (match-string-no-properties 1))))
          (if (> ver 100)
              (/ ver 100)
            ver))
      0)))

(defun company-clang-objc-templatify (selector)
  (let* ((end (point-marker))
         (beg (- (point) (length selector) 1))
         (templ (company-template-declare-template beg end))
         (cnt 0))
    (save-excursion
      (goto-char beg)
      (catch 'stop
        (while (search-forward ":" end t)
          (when (looking-at "([^)]*) ?")
            (delete-region (match-beginning 0) (match-end 0)))
          (company-template-add-field templ (point) (format "arg%d" cnt))
          (if (< (point) end)
              (insert " ")
            (throw 'stop t))
          (cl-incf cnt))))
    (company-template-move-to-first templ)))

(defun company-clang (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Clang.
Clang is a parser for C and ObjC.  Clang version 1.1 or newer is required.

Additional command line arguments can be specified in
`company-clang-arguments'.  Prefix files (-include ...) can be selected
with `company-clang-set-prefix' or automatically through a custom
`company-clang-prefix-guesser'.

With Clang versions before 2.9, we have to save the buffer before
performing completion.  With Clang 2.9 and later, buffer contents are
passed via standard input."
  (interactive (list 'interactive))
  ;; BUGTESTING (time measurement)
  ;; ----------
  (when (and (eq time-start nil)
             (eq command 'prefix))
    (setq time-start (current-time-pico))
    (message "prefix: %s" time-start))
  ;; ----------
  (cl-case command
    (interactive (company-begin-backend 'company-clang))
    (init (when (memq major-mode company-clang-modes)
            (unless company-clang-executable
              (error "Company found no clang executable"))
            (setq company-clang--version (company-clang-version))
            (when (< company-clang--version company-clang-required-version)
              (error "Company requires clang version 1.1"))
            (when (and company-clang-parse-comments
                       (not (company-clang--can-parse-comments)))
              (error "The current version of Clang cannot parse comments"))))
    (prefix (and (memq major-mode company-clang-modes)
                 buffer-file-name
                 company-clang-executable
                 (not (company-in-string-or-comment))
                 (or (company-clang--prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb) (company-clang--candidates arg cb))))
    (meta       (company-clang--meta arg))
    (annotation (company-clang--annotation arg))
    (doc-buffer (company-clang--doc-buffer arg))
    (post-completion (let ((anno (company-clang--annotation arg)))
                       (when (and company-clang-insert-arguments anno)
                         (insert anno)
                         (if (string-match "\\`:[^:]" anno)
                             (company-clang-objc-templatify anno)
                           (company-template-c-like-templatify
                            (concat arg anno))))))))

;; BUGTESTING (time measurement)
;; ----------
(defun company-clang-calc-deltas-media nil
  "Calculate the media of the time measurements gathered in the
current buffer.

Print the result at the end of the current buffer.

Be sure that the time measurements are correct, sometimes the
first data printed are incorrect, so remove them from the buffer
before calling this function."
  (interactive)
  (let ((content (buffer-string))
        (buf (current-buffer))
        (filter "^%s: \\(.*\\)$")
        sum count results)
    (with-temp-buffer
      (insert content)
      (dolist (item (list "process-delta"
                          "parse-delta"
                          "delta"
                          "ast-delta"))
        (setq sum 0)
        (setq count 0)
        (goto-char (point-min))
        (while (re-search-forward (format filter item) nil t)
          (setq count (+ count 1))
          (setq sum (+ sum
                       (string-to-number
                        (match-string-no-properties 1)))))
        (when (and (> sum 0) (> count 0))
          (setq sum (/ sum count)))
        (setq results
              (append
               results
               (list (concat item ": " (number-to-string sum)))))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n")
      (dolist (item results)
        (insert (concat item "\n"))))))
;; ----------

(provide 'company-clang)
;;; company-clang.el ends here
