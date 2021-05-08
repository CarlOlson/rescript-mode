;;; rescript-indent.el --- Indentation functions for ReScript -*-lexical-binding: t-*-

;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Indentation functions for ReScript.

;;; Code:

(defconst rescript-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defcustom rescript-indent-offset 2
  "Indent ReScript code by this number of spaces."
  :type 'integer
  :group 'rescript-mode
  :safe #'integerp)

(defun rescript-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps.
Works around some regexp slowness.
Argument STR string to search for."
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun rescript-paren-level ()
  "Get the level of nesting inside parentheses."
  (nth 0 (syntax-ppss)))

(defun rescript-in-str-or-cmnt ()
  "Return whether point is currently inside a string or a comment."
  (nth 8 (syntax-ppss)))

(defun rescript-rewind-past-str-cmnt ()
  "Rewind past string or comment."
  (goto-char (nth 8 (syntax-ppss))))

(defun rescript-rewind-irrelevant ()
  "Rewind past irrelevant characters (whitespace of inside comments)."
  (interactive)
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (rescript-looking-back-str "*/") (backward-char))
    (if (rescript-in-str-or-cmnt)
        (rescript-rewind-past-str-cmnt))
    (if (/= starting (point))
        (rescript-rewind-irrelevant))))

(defun rescript-align-to-expr-after-brace ()
  "Align the expression at point to the expression after the previous brace."
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))

(defun rescript-align-to-prev-expr ()
  "Align the expression at point to the previous expression."
  (let ((alignment (save-excursion
                     (forward-char)
                     ;; We don't want to indent out to the open bracket if the
                     ;; open bracket ends the line
                     (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
                       (if (looking-at "[[:space:]]")
                           (progn
                             (forward-word 1)
                             (backward-word 1))
                         (backward-char))
                       (current-column)))))
    (if (not alignment)
        (save-excursion
          (forward-char)
          (forward-line)
          (back-to-indentation)
          (current-column))
      alignment)))

;;; Start of a rescript binding
(defvar rescript-binding
  (regexp-opt '("let" "type" "module" "fun")))

(defun rescript-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function'.
Don't move to the beginning of the line.  `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" rescript-binding "\\)\\_>")
                      nil 'move (or arg 1)))

(defun rescript-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after ‘beginning-of-defun’.  So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for ReScript."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

(defun rescript-rewind-to-beginning-of-current-level-expr ()
  "Rewind to the beginning of the expression on the current level of nesting."
  (interactive)
  (let ((current-level (rescript-paren-level)))
    (back-to-indentation)
    (when (looking-at "=>")
      (rescript-rewind-irrelevant)
      (back-to-indentation))
    (while (> (rescript-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))))

(defun rescript-mode-indent-line ()
  "Indent current line."
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (rescript-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket. That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                     (save-excursion
                       ;; jsx, end of previous tag
                       (rescript-rewind-irrelevant)
                       (if (save-excursion
                             (rescript-rewind-to-beginning-of-current-level-expr)
                             ;; beginning of previous tag
                             (looking-at "\\(<\\|\\.\\.\\.\\)"))
                           (progn
                             (rescript-rewind-to-beginning-of-current-level-expr)
                             ;; beginning of previous tag
                             (cond
                              ((looking-at ".*\\(<.*/>\\|</.*>\\)") ;; (self) closing tag
                               (current-column))
                              ((looking-at "<")
                               (+ (current-column) rescript-indent-offset))
                              (t (current-column))))
                           (progn
                             (unless (and (looking-at "[[:space:]\n]*<")
                                          (rescript-looking-back-str "=>"))
                               (backward-up-list))
                             (rescript-rewind-to-beginning-of-current-level-expr)

                             (cond
                              ((looking-at "switch")
                               (current-column))

                              ((looking-at "if")
                               (+ (current-column) rescript-indent-offset))

                              ((looking-at "|")
                               (+ (current-column) (* rescript-indent-offset 2)))

                              ((looking-at "[[:word:]]+:.*=> ?{?$")
                               (+ (current-column) rescript-indent-offset))

                              (t
                               (let ((current-level (rescript-paren-level)))
                                 (save-excursion
                                   (while (and (= current-level (rescript-paren-level))
                                               (not (looking-at rescript-binding)))
                                     (rescript-rewind-irrelevant)
                                     (rescript-rewind-to-beginning-of-current-level-expr))
                                   (+ (current-column) rescript-indent-offset)))))))))))
             (cond
              ;; A function return type is indented to the corresponding function arguments
              ((looking-at "=>")
               (+ baseline rescript-indent-offset))

              ((rescript-in-str-or-cmnt)
               (cond
                ;; In the end of the block -- align with star
                ((looking-at "*/") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;  * asdf
                ;;  */
                ;;
                ((looking-at "*") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;    asdf
                ;;  */
                ;;
                (t (+ baseline (+ rescript-indent-offset 1)))))

              ((looking-at "</") (- baseline rescript-indent-offset))
              ((looking-at "<") baseline)
              ((looking-at "<.*/>") baseline)
              ((looking-at "\\.\\.\\.") baseline)

              ;; A closing brace is 1 level unindented
              ((looking-at "}\\|)\\|\\]")
               (save-excursion
                 (rescript-rewind-irrelevant)
                 (let ((jsx? (rescript-looking-back-str ">")))
                   (backward-up-list)
                   (rescript-rewind-to-beginning-of-current-level-expr)
                   (cond
                    ((looking-at "switch") baseline)

                    (jsx? (current-column))

                    (t (- baseline rescript-indent-offset))))))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (rescript-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (rescript-align-to-expr-after-brace)))

                (progn
                  (back-to-indentation)
                  (cond ((looking-at (regexp-opt '("and" "type")))
                         baseline)
                        ((save-excursion
                           (rescript-rewind-irrelevant)
                           (= (point) 1))
                         baseline)
                        ((save-excursion
                           (while (looking-at "|")
                             (rescript-rewind-irrelevant)
                             (back-to-indentation))
                           (looking-at (regexp-opt '("type"))))
                         (+ baseline rescript-indent-offset))
                        ((looking-at "|\\|/[/*]")
                         baseline)
                        ((and (> level 0)
                              (save-excursion
                                (rescript-rewind-irrelevant)
                                (backward-up-list)
                                (rescript-rewind-to-beginning-of-current-level-expr)
                                (looking-at "switch")))
                         (+ baseline rescript-indent-offset))
                        ((save-excursion
                           (rescript-rewind-irrelevant)
                           (looking-back "[{;,\\[(]" (- (point) 2)))
                         baseline)
                        ((and
                          (save-excursion
                            (rescript-rewind-irrelevant)
                            (rescript-rewind-to-beginning-of-current-level-expr)
                            (and (looking-at rescript-binding)
                                 (not (progn
                                        (forward-sexp)
                                        (forward-sexp)
                                        (skip-chars-forward "[:space:]\n")
                                        (looking-at "=")))))
                          (not (save-excursion
                                 (skip-chars-backward "[:space:]\n")
                                 (rescript-looking-back-str "=>"))))
                         (save-excursion
                           (rescript-rewind-irrelevant)
                           (backward-sexp)
                           (rescript-align-to-prev-expr)))
                        ((save-excursion
                           (rescript-rewind-irrelevant)
                           (looking-back "<\/.*?>" (- (point) 30)))
                         baseline)
                        (t
                         (save-excursion
                           (rescript-rewind-irrelevant)
                           (rescript-rewind-to-beginning-of-current-level-expr)

                           (if (looking-at "|")
                               baseline
                             (+ baseline rescript-indent-offset)))))
                  ;; Point is now at the beginning of the current line
                  ))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(provide 'rescript-indent)

;;; rescript-indent.el ends here
