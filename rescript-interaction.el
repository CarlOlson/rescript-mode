;;; rescript-interaction.el --- Phrase navitagion for rtop -*-lexical-binding: t-*-

;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Phrase navigation for utop and maybe other REPLs.

;; The utop compatibility layer for ReScript was mainly taken from:
;; https://github.com/ocaml/tuareg/blob/master/tuareg-light.el (big thanks!)

;;; Code:

(defun rescript-backward-char (&optional step)
  "Go back one char.
Similar to `backward-char` but it does not signal errors
`beginning-of-buffer` and `end-of-buffer`.  It optionally takes a
STEP parameter for jumping back more than one character."
  (when step (goto-char (- (point) step))
        (goto-char (1- (point)))))

(defun rescript-forward-char (&optional step)
  "Go forward one char.
Similar to `forward-char` but it does not signal errors
`beginning-of-buffer` and `end-of-buffer`.  It optionally takes a
STEP parameter for jumping back more than one character."
  (when step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun rescript-in-literal-p ()
  "Return non-nil if point is inside an ReScript literal."
  (nth 3 (syntax-ppss)))

(defconst rescript-comment-delimiter-regexp "\\*/\\|/\\*"
  "Regex for identify either open or close comment delimiters.")

(defun rescript-in-between-comment-chars-p ()
  "Return non-nil iff point is in between the comment delimiter chars.
It returns non-nil if point is between the chars only (*|/ or /|*
where | is point)."
  (and (not (bobp)) (not (eobp))
       (or (and (char-equal ?/ (char-before)) (char-equal ?* (char-after)))
           (and (char-equal ?* (char-before)) (char-equal ?/ (char-after))))))

(defun rescript-looking-at-comment-delimiters-p ()
  "Return non-nil iff point in between comment delimiters."
  (looking-at-p rescript-comment-delimiter-regexp))

(defun rescript-in-between-comment-delimiters-p ()
  "Return non-nil if inside /* and */."
  (nth 4 (syntax-ppss)))

(defun rescript-in-comment-p ()
  "Return non-nil iff point is inside or right before a comment."
  (or (rescript-in-between-comment-delimiters-p)
      (rescript-in-between-comment-chars-p)
      (rescript-looking-at-comment-delimiters-p)))

(defun rescript-beginning-of-literal-or-comment ()
  "Skip to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (goto-char (or (nth 8 (syntax-ppss)) (point))))

(defun rescript-inside-block-scope-p ()
  "Skip to the beginning of the current literal or comment (or buffer)."
  (and (> (nth 0 (syntax-ppss)) 0)
       (let ((delim-start (nth 1 (syntax-ppss))))
         (save-excursion
           (goto-char delim-start)
           (char-equal ?{ (following-char))))))

(defun rescript-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  ;; Difference from OCaml, the phrase separator is a single semi-colon
  (and (not (eobp))
       (char-equal ?\; (following-char))))

(defun rescript-skip-to-close-delimiter (&optional limit)
  "Skip to the end of a ReScript block.
It basically calls `re-search-forward` in order to go to any
closing delimiter, not concerning itself with balancing of any
sort.  Client code needs to check that.
LIMIT is passed to `re-search-forward` directly."
  (re-search-forward "\\s)" limit 'move))

(defun rescript-skip-back-to-open-delimiter (&optional limit)
  "Skip to the beginning of a ReScript block backwards.
It basically calls `re-search-backward` in order to go to any
opening delimiter, not concerning itself with balancing of any
sort.  Client code needs to check that.
LIMIT is passed to `re-search-backward` directly."
  (re-search-backward "\\s(" limit 'move))

(defun rescript-find-phrase-end ()
  "Skip to the end of a phrase."
  (while (and (not (eobp))
              (not (rescript-at-phrase-break-p)))
    (if (re-search-forward ";" nil 'move)
        (progn (when (rescript-inside-block-scope-p)
                 (rescript-skip-to-close-delimiter))
               (goto-char (1- (point))))
      ;; avoid infinite loop at the end of the buffer
      (re-search-forward "[[:space:]\\|\n]+" nil 'move)))
  (min (goto-char (1+ (point))) (point-max)))

(defun rescript-skip-blank-and-comments ()
  "Skip blank spaces and comments."
  (cond
   ((eobp) (point))
   ((or (rescript-in-between-comment-chars-p)
        (rescript-looking-at-comment-delimiters-p)) (progn
                                                    (rescript-forward-char 1)
                                                    (rescript-skip-blank-and-comments)))
   ((rescript-in-between-comment-delimiters-p) (progn
                                               (search-forward "*/" nil t)
                                               (rescript-skip-blank-and-comments)))
   ((eolp) (progn
             (rescript-forward-char 1)
             (rescript-skip-blank-and-comments)))
   (t (progn (skip-syntax-forward " ")
             (point)))))

(defun rescript-skip-back-blank-and-comments ()
  "Skip blank spaces and comments backwards."
  (cond
   ((bobp) (point))
   ((looking-back rescript-comment-delimiter-regexp) (progn
                                                     (rescript-backward-char 1)
                                                     (rescript-skip-back-blank-and-comments)))
   ((rescript-in-between-comment-delimiters-p) (progn
                                               (search-backward "/*" nil t)
                                               (rescript-backward-char 1)
                                               (rescript-skip-back-blank-and-comments)))
   ((or (rescript-in-between-comment-chars-p)
        (rescript-looking-at-comment-delimiters-p)) (progn
                                                    (rescript-backward-char 1)
                                                    (rescript-skip-back-blank-and-comments)))
   ((bolp) (progn
             (rescript-backward-char 1)
             (rescript-skip-back-blank-and-comments)))
   (t (progn (skip-syntax-backward " ")
             (point)))))

(defun rescript-ro (&rest words)
  "Build a regex matching iff at least a word in WORDS is present."
  (concat "\\<" (regexp-opt words t) "\\>"))

(defconst rescript-find-phrase-beginning-regexp
  (concat (rescript-ro "end" "type" "module" "sig" "struct" "class"
                     "exception" "open" "let")
          "\\|^#[ \t]*[a-z][_a-z]*\\>\\|;"))

(defun rescript-at-phrase-start-p ()
  "Return t if is looking at the beginning of a phrase.
A phrase starts when a toplevel keyword is at the beginning of a line."
  (or (looking-at "#")
      (looking-at rescript-find-phrase-beginning-regexp)))

(defun rescript-find-phrase-beginning-backward ()
  "Find the beginning of a phrase and return point.
It scans code backwards, therefore the caller can assume that the
beginning of the phrase (if found) is always before the starting
point.  No error is signalled and (point-min) is returned when a
phrease cannot be found."
  (beginning-of-line)
  (while (and (not (bobp)) (not (rescript-at-phrase-start-p)))
    (if (rescript-inside-block-scope-p)
        (rescript-skip-back-to-open-delimiter)
      (re-search-backward rescript-find-phrase-beginning-regexp nil 'move)))
  (point))

(defun rescript-discover-phrase ()
  "Discover a ReScript phrase in the buffer."
  ;; TODO rescript-with-internal-syntax ;; tuareg2 modifies the syntax table (removed for now)
  ;; TODO stop-at-and feature for phrase detection (do we need it?)
  ;; TODO tuareg2 has some custom logic for module and class (do we need it?)
  (save-excursion
    (let ((case-fold-search nil))
      (rescript-skip-blank-and-comments)
      (list (rescript-find-phrase-beginning-backward) ;; beginning
            (rescript-find-phrase-end)                ;; end
            (save-excursion                         ;; end-with-comment
              (rescript-skip-blank-and-comments)
              (point))))))

(defun rescript-discover-phrase-debug ()
  "Discover a ReScript phrase in the buffer (debug mode)."
  (let ((triple (rescript-discover-phrase)))
    (message (concat "Evaluating: \"" (rescript-fetch-phrase triple) "\""))
    triple))

(defun rescript-fetch-phrase (triple)
  "Fetch the phrase text given a TRIPLE."
  (let* ((start (nth 0 triple))
         (end (nth 1 triple))) ;; we don't need end-with-comment
    (buffer-substring-no-properties start end)))

(defun rescript-next-phrase ()
  "Skip to the beginning of the next phrase."
  (cond
   ((rescript-at-phrase-start-p) (point))
   ((eolp) (progn
             (forward-char 1)
             (rescript-skip-blank-and-comments)
             (rescript-next-phrase)))
   ((rescript-inside-block-scope-p) (progn (rescript-skip-to-close-delimiter)
                                         (rescript-next-phrase)))
   ((looking-at ";") (progn
                       (forward-char 1)
                       (rescript-next-phrase)))
   (t (progn (end-of-line)
             (rescript-next-phrase)))))

(provide 'rescript-interaction)

;;; rescript-interaction.el ends here
