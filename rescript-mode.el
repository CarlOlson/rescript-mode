;;; rescript-mode.el --- A major mode for editing ReScript -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Version: 0.4.0
;; Author: Mozilla
;; Url: https://github.com/reasonml-editor/reason-mode
;; Keywords: languages, ocaml
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;; This project provides useful functions and helpers for developing
;; code using the ReScript programming language.
;;
;; It offers:
;;  - A new, familiar syntax for the battle-tested language that is OCaml.
;;  - A workflow for compiling to JavaScript and native code.
;;  - A set of friendly documentations, libraries and utilities.
;;
;; See the README.md for more details.

;;; Code:

(require 'rescript-indent)
(require 'rescript-interaction)

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

;; Syntax definitions and helpers
(defvar rescript-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "_"  table)
    (modify-syntax-entry ?\` "\"" table)

    ;; Comments
    (modify-syntax-entry ?\/  ". 124b" table)
    (modify-syntax-entry ?\*  ". 23n"  table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)

    ;; Quoted variable names
    (modify-syntax-entry ?\\ "\\ p" table)

    table))

(defgroup rescript nil
  "Support for ReScript code."
  :link '(url-link "https://rescript-lang.org/")
  :group 'languages)

(defcustom rescript-mode-hook nil
  "Hook called by `rescript-mode'."
  :type 'hook
  :group 'rescript)

;; Font-locking definitions and helpers
(defconst rescript-mode-keywords
  (split-string
   "and as assert constraint else exception external for if in
include lazy let module mutable of open rec switch try type when
while with"))

(defconst rescript-mode-consts
  '("true" "false"))

(defconst rescript-special-types
  (split-string
   "int float string char bool unit list array exn option ref"))

(defconst rescript-camel-case
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(eval-and-compile
  (defconst rescript--char-literal-rx
    (rx (seq (group "'")
             (or (seq "\\" anything)
                 (not (any "'\\")))
             (group "'")))))

(defun rescript-re-word (inner)
  "Build a word regexp given INNER."
  (concat "\\<" inner "\\>"))

(defun rescript-re-grab (inner)
  "Build a grab regexp given INNER."
  (concat "\\(" inner "\\)"))

(defun rescript-regexp-opt-symbols (words)
  "Like `(regexp-opt words 'symbols)`, but will work on Emacs 23.
See rust-mode PR #42.
Argument WORDS argument to pass to `regexp-opt`."
  (concat "\\_<" (regexp-opt words t) "\\_>"))

;;; Syntax highlighting for ReScript
(defvar rescript-font-lock-keywords
  `((,(rescript-regexp-opt-symbols rescript-mode-keywords) . font-lock-keyword-face)
    (,(rescript-regexp-opt-symbols rescript-special-types) . font-lock-builtin-face)
    (,(rescript-regexp-opt-symbols rescript-mode-consts) . font-lock-constant-face)

    (,rescript-camel-case 1 font-lock-type-face)

    ;; Field names like `foo:`, highlight excluding the :
    (,(concat (rescript-re-grab rescript-re-ident) ":[^:]") 1 font-lock-variable-name-face)
    ;; Module names like `foo::`, highlight including the ::
    (,(rescript-re-grab (concat rescript-re-ident "::")) 1 font-lock-type-face)
    ;; Name punned labeled args like ::foo
    (,(concat "[[:space:]]+" (rescript-re-grab (concat "::" rescript-re-ident))) 1 font-lock-type-face)

    ;; TODO jsx attribs?
    (,
     (concat "<[/]?" (rescript-re-grab rescript-re-ident) "[^>]*" ">")
     1 font-lock-type-face)))

(defun rescript-mode-try-find-alternate-file (mod-name extension)
  "Switch to the file given by MOD-NAME and EXTENSION."
  (let* ((filename (concat mod-name extension))
         (buffer (get-file-buffer filename)))
    (if buffer (switch-to-buffer buffer)
      (find-file filename))))

(defun rescript-mode-find-alternate-file ()
  "Switch to implementation/interface file."
  (interactive)
  (let ((name buffer-file-name))
    (when (string-match "\\`\\(.*\\)\\.re\\([il]\\)?\\'" name)
      (let ((mod-name (match-string 1 name))
            (e (match-string 2 name)))
        (cond
         ((string= e "i")
          (rescript-mode-try-find-alternate-file mod-name ".re"))
         (t
          (rescript-mode-try-find-alternate-file mod-name ".rei")))))))

(defun rescript-syntax-propertize-function (start end)
  "Propertize ReScript function.
Argument START marks the beginning of the function.
Argument END marks the end of the function."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    (rescript--char-literal-rx (1 "\"") (2 "\"")))
   (point) end))

(defvar rescript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'rescript-mode-find-alternate-file)
    map))

;;;###autoload
(define-derived-mode rescript-mode prog-mode "ReScript"
  "Major mode for ReScript code.

\\{rescript-mode-map}"
  :group 'rescript
  :syntax-table rescript-mode-syntax-table
  :keymap rescript-mode-map

  ;; Syntax
  (setq-local syntax-propertize-function #'rescript-syntax-propertize-function)
  ;; Indentation
  (setq-local indent-line-function 'rescript-mode-indent-line)
  ;; Fonts
  (setq-local font-lock-defaults '(rescript-font-lock-keywords))
  ;; Misc
  (setq-local comment-start "/* ")
  (setq-local comment-end   " */")
  (setq-local indent-tabs-mode nil)
  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local paragraph-start
              (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local require-final-newline t)
  (setq-local normal-auto-fill-function nil)
  (setq-local comment-multi-line t)

  (setq-local beginning-of-defun-function 'rescript-beginning-of-defun)
  (setq-local end-of-defun-function 'rescript-end-of-defun)
  (setq-local parse-sexp-lookup-properties t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.resi?$" . rescript-mode))

(defun rescript-mode-reload ()
  "Reload ReScript mode."
  (interactive)
  (unload-feature 'rescript-mode)
  (unload-feature 'rescript-indent)
  (unload-feature 'rescript-interaction)
  (require 'rescript-mode)
  (rescript-mode))

(provide 'rescript-mode)

;;; rescript-mode.el ends here
