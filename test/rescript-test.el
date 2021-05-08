;;; rescript-mode-tests.el --- ERT tests for rescript-mode.el
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Tests for rescript-mode.

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(require 'ert-x)
(require 'rescript-mode)
(require 'cl-lib)

(setq rescript-test-fill-column 32)

(defun rescript-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun rescript-test-explain-bad-manip (original point-pos manip-func expected got)
  (if (equal expected got)
      nil
    (list
     ;; The (goto-char) and (insert) business here is just for
     ;; convenience--after an error, you can copy-paste that into emacs eval to
     ;; insert the bare strings into a buffer
     "ReScript code was manipulated wrong after:"
     `(insert ,original)
     `(goto-char ,point-pos)
     'expected `(insert ,expected)
     'got `(insert ,got)
     (cl-loop for i from 0 to (max (length original) (length expected))
              for oi = (if (< i (length got)) (elt got i))
              for ei = (if (< i (length expected)) (elt expected i))
              while (equal oi ei)
              finally return `(first-difference-at
                               (goto-char ,(+ 1 i))
                               expected ,(char-to-string ei)
                               got ,(char-to-string oi))))))

(put 'rescript-compare-code-after-manip
     'ert-explainer
     'rescript-test-explain-bad-manip)

(defun rescript-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (rescript-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (rescript-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

(defun test-indent (indented &optional deindented)
  (let ((deindented (or deindented (replace-regexp-in-string "^[[:blank:]]*" "      " indented))))
    (rescript-test-manip-code
     deindented
     1
     (lambda ()
       ;; The indentation will fail in some cases if the syntax properties are
       ;; not set.  This only happens when font-lock fontifies the buffer.
       (font-lock-fontify-buffer)
       (indent-region 1 (+ 1 (buffer-size))))
     indented)))

(ert-deftest indent-struct-fields-aligned ()
  (test-indent "
type foo = { bar: int,
             baz: int};

type blah = {x:int,
             y:int,
             z:string};"))

(ert-deftest indent-top-level ()
  (test-indent "
/* Everything here is at the top level and should not be indented*/
let greeting = \"hello!\";
let score = 10;
let newScore = 10 + score;
"))

;; TODO how to align these
(ert-deftest indent-params-no-align ()
  (test-indent "
/* Indent out one level because no params appear on the first line */
fun xyzzy(
  a:int,
  b:char) => {};

fun abcdef(
  a:int,
  b:char)
  :int =>
  { 1 };
"))

(ert-deftest indent-params-align1 ()
  (test-indent "
/* Align the second line of params to the first */
fun foo(a:int,
        b:char) => {};
"))

(ert-deftest indent-params-align2 ()
  (test-indent "
/* Align the second line of params to the first */
fun foo2(   a:int,
            b:char)
            :int =>
  { 1 };
"))

(ert-deftest indent-params-align3 ()
  (test-indent "
/* Align the second line of params to the first */
fun foo3(   a:int,  /* should work with a comment here */
            b:char)
            :int =>
  { 1 };
"))

(ert-deftest indent-open-after-arrow1 ()
  (test-indent "
/* Indent function body only one level after `=> {` */
fun foo1(a:int) (b:char) :int => {
  let body = \"hello\";
  1
};
"))

(ert-deftest indent-open-after-arrow2 ()
  (test-indent "
/* Indent function body only one level after `=> {` */
fun foo2 (a:int)
         (b:char) :int => {
  let body = \"hello\";
  1
};
"))

(ert-deftest indent-open-after-arrow3 ()
  (test-indent "
/* Indent function body only one level after `=> {` */
fun foo3(a:int,
         b:char)
        :int => {
  let body = \"hello\";
  1
};
"))

(ert-deftest indent-square-bracket-alignment ()
  (test-indent "
fun args_on_the_next_line( /* with a comment */
                              a:int,
                              b:String) => {
  let aaaaaa = [
    1,
    2,
    3];
  let bbbbbbb = [1, 2, 3,
                 4, 5, 6];
  let ccc = [   10, 9, 8,
                7, 6, 5];
};
"))

;; TODO: uncomment
;; TODO fix alignment of curly braces when down a line
;; (ert-deftest indent-multi-line-expr ()
;;   (test-indent
;;    "
;; fun foo() =>
;;   {
;;     x();
;;     let a =
;;       b()
;;   };
;; "))

(ert-deftest indent-switch ()
  (test-indent "
fun foo() => {
  switch blah {
  | Pattern => stuff()
  | _ => whatever
  }
};
"))

(ert-deftest indent-if ()
  (test-indent "
fun foo() => {
  if (blah) {
    stuff
  } else {
    otherStuff
  }
}"))

(ert-deftest indent-switch-multiline-pattern ()
  (test-indent "
fun foo() => {
  switch blah {
  | Pattern => \"dada\"
  | Pattern2 => {
      hello()
    }
  | _ => \"whatever\"
  }
};
"))

(ert-deftest indent-normal-switch ()
  (test-indent "
let hasExactlyTwoCars lst =>
  switch lst {
  | NoMore => false                              /* 0 */
  | List p NoMore => false                       /* 1 */
  | List p (List p2 NoMore) => true              /* 2 */
  | List p (List p2 (List p3 theRest)) => false  /* 3+ */
  };
"))

(ert-deftest indent-indented-switch ()
  (test-indent "
fun foo() => {
  let x = {
    switch blah {
    | Pattern => \"dada\"
    | Pattern2 => {
        hello()
      }
    | _ => \"whatever\"
    }
  };
  y();
};
"))

(ert-deftest indent-indented-object-func ()
  (test-indent "
module MyApp = {
  type state = {db:db};
  type action = Click;
  let component = ReScriptReact.reducerComponent(\"MyApp\");
  let make = (_children) => {
    ...component,
    initialState: () => {db:[||]},
    reducer: (a: action, s:state) =>
      switch(a) {
      | Click => ReScriptReact.Update(s)
      },
    render: _self => {
      let a = 20;
      <Text value=\"foo\" />
    }
  }
};
"))

(ert-deftest indented-multi-expr-switch ()
  (test-indent "
fun foo() => {
  let x = {
    switch blah {
    | Pattern => \"dada\"
    | Pattern2 =>
      hello();
      other();
    | _ => \"whatever\"
    }
  };
  y();
};
"))

;; Make sure that in effort to cover switch patterns we don't mistreat || or expressions
(ert-deftest indent-nonswitch-or-expression ()
  (test-indent "
fun foo() => {
  let x = foo() ||
    bar();
};
"))

;; Closing braces in single char literals and strings should not confuse the indentation
;; TODO In ReScript it does confuse indentation
(ert-deftest indent-closing-braces-in-char-literals ()
  (test-indent "
fun foo() => {
  bar('}');
  bar(']');
  bar(')');
};
"))

(ert-deftest indent-jsx ()
  (test-indent "
fun foo() => {
  <div attr=\"bar\">
    <img src=\"foo.png\"/>
  </div>
};
"))

(ert-deftest indent-jsx-2 ()
  (test-indent "
let make keyInfo::k=? _children => {
  ...component,
  render: fun _ =>
    <div className=\"App\">
      <div className=\"header\">
        <h2> foobar </h2>
        <h3> bar baz quz </h3>
      </div>
      <div className=\"footer\">
        <p> more here </p>
      </div>
    </div>
};
"))

(ert-deftest indent-jsx-3 ()
  (test-indent "
let make = (_children) => {
  ...component,
  render: self => {
    let name = \"foo\";
    let children = List.map(el => <Text value=el />, [\"foo\", \"bar\"]);
    <View>
      <Text value=name />
      <View>
        ...children
      </View>
    </View>
  }
};
"))

(ert-deftest indent-jsx-4 ()
  (test-indent "
let make = (name, children) => {
  ...component,
  render: self =>
    <View>
      <Text value=name />
      <View>
        ...children
      </View>
    </View>
};
"))

(defun rescript-get-buffer-pos (pos-symbol)
  "Get buffer position from POS-SYMBOL.

POS-SYMBOL is a symbol found in `rescript-test-positions-alist'.
Convert the line-column information from that list into a buffer position value."
  (interactive "P")
  (let* (
         (line-and-column (cadr (assoc pos-symbol rescript-test-positions-alist)))
         (line (nth 0 line-and-column))
         (column (nth 1 line-and-column)))
    (save-excursion
      (goto-line line)
      (move-to-column column)
      (point))))

(defun rescript-test-fontify-string (str)
  (with-temp-buffer
    (rescript-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun rescript-test-group-str-by-face (str)
  "Fontify `STR' in rescript-mode and group it by face, returning a
list of substrings of `STR' each followed by its face."
  (cl-loop with fontified = (rescript-test-fontify-string str)
           for start = 0 then end
           while start
           for end   = (next-single-property-change start 'face fontified)
           for prop  = (get-text-property start 'face fontified)
           for text  = (substring-no-properties fontified start end)
           if prop
           append (list text prop)))

(defun rescript-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'"
  (should (equal (rescript-test-group-str-by-face source)
                 face-groups)))

(ert-deftest font-lock-attribute-inside-string ()
  (rescript-test-font-lock
   "\"#[foo]\""
   '("\"#[foo]\"" font-lock-string-face)))

(ert-deftest font-lock-attribute-inside-comment ()
  (rescript-test-font-lock
   "/* #[foo] */"
   '("/* " font-lock-comment-delimiter-face
     "#[foo] " font-lock-comment-face
     "*/" font-lock-comment-delimiter-face)))

(ert-deftest font-lock-double-quote-character-literal ()
  (rescript-test-font-lock
   "'\"'; let"
   '("'\"'" font-lock-string-face
     "let" font-lock-keyword-face)))

(ert-deftest font-lock-single-quote-character-literal ()
  (rescript-test-font-lock
   "let main = () => { let ch = '\\''; }"
   '("let" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\''" font-lock-string-face)))

(ert-deftest font-lock-escaped-double-quote-character-literal ()
  (rescript-test-font-lock
   "let main = () => { let ch = '\\\"'; }"
   '("let" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\\"'" font-lock-string-face)))

(ert-deftest font-lock-escaped-backslash-character-literal ()
  (rescript-test-font-lock
   "let main = () => { let ch = '\\\\'; }"
   '("let" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\\\'" font-lock-string-face)))

(ert-deftest font-lock-string-ending-with-r-not-raw-string ()
  (rescript-test-font-lock
   "let f = () => {
    \"Er\";
};

let g = () => {
    \"xs\";
};"
   '("let" font-lock-keyword-face
     "\"Er\"" font-lock-string-face
     "let" font-lock-keyword-face
     "\"xs\"" font-lock-string-face)))

(ert-deftest rescript-test-two-character-quotes-in-a-row ()
  (with-temp-buffer
    (rescript-mode)
    (font-lock-fontify-buffer)
    (insert "'\\n','a', let")
    (font-lock-after-change-function 1 12 0)

    (should (equal 'font-lock-string-face (get-text-property 3 'face)))
    (should (equal nil (get-text-property 5 'face)))
    (should (equal 'font-lock-string-face (get-text-property 7 'face)))
    (should (equal nil (get-text-property 9 'face)))
    (should (equal 'font-lock-keyword-face (get-text-property 12 'face)))))

(ert-deftest single-quote-null-char ()
  (rescript-test-font-lock
   "'\\0' 'a' let"
   '("'\\0'" font-lock-string-face
     "'a'" font-lock-string-face
     "let" font-lock-keyword-face)))

(ert-deftest r-in-string-after-single-quoted-double-quote ()
  (rescript-test-font-lock
   "'\"';\n\"r\";\n\"oops\";"
   '("'\"'" font-lock-string-face
     "\"r\"" font-lock-string-face
     "\"oops\"" font-lock-string-face)))

(ert-deftest quoted-variable-names ()
  (rescript-test-font-lock
   "\\\"foo\""
   '("\"foo\"" font-lock-string-face)))

(ert-deftest backtick-string ()
  (rescript-test-font-lock
   "`foo`"
   '("`foo`" font-lock-string-face)))

;; This may be too complex to pass
;; (ert-deftest backtick-string-interpolation ()
;;   (rescript-test-font-lock
;;    "`${foo}`"
;;    '("`" font-lock-string-face
;;      "${foo}" font-lock-variable-name-face
;;      "`" font-lock-string-face)))
