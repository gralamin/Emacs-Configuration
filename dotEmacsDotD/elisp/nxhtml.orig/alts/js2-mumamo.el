;;; js2.el -- an improved JavaScript editing mode

;; THIS VERSION IS MODIFIED FOR MUMAMO

;;;
;;; This file was auto-generated on Wed May 21 17:14:56 2008 from files:
;;;  js2-vars.el
;;;  js2-util.el
;;;  js2-scan.el
;;;  js2-messages.el
;;;  js2-ast.el
;;;  js2-highlight.el
;;;  js2-browse.el
;;;  js2-parse.el
;;;  js2-indent.el
;;;  js2-mode.el

;;; js2-mode.el --- an improved JavaScript editing mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Version: 20080521
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This JavaScript editing mode supports:
;;
;;  - the full JavaScript language through version 1.7
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - syntax-error and strict-mode warning reporting
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments (Emacs 22+) and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - code browsing using the `imenu' package
;;  - typing helpers (e.g. inserting matching braces/parens)
;;  - many customization options
;;
;; It is only compatible with GNU Emacs versions 21 and higher (not XEmacs).
;;
;; Installation:
;;
;;  - put `js2.el' somewhere in your emacs load path
;;  - M-x byte-compile-file RET <path-to-js2.el> RET
;;    Note:  it will refuse to run unless byte-compiled
;;  - add these lines to your .emacs file:
;;    (autoload 'js2-mode "js2" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;
;; To customize how it works:
;;   M-x customize-group RET js2-mode RET
;;
;; The variable `js2-mode-version' is a date stamp.  When you upgrade
;; to a newer version, you must byte-compile the file again.
;;
;; Notes:
;;
;; This mode is different in many ways from standard Emacs language editing
;; modes, inasmuch as it attempts to be more like an IDE.  If this drives
;; you crazy, it IS possible to customize it to be more like other Emacs
;; editing modes.  Please customize the group `js2-mode' to see all of the
;; configuration options.
;;
;; Some of the functionality does not work in Emacs 21 -- upgrading to
;; Emacs 22 or higher will get you better results.  If you byte-compiled
;; js2.el with Emacs 21, you should re-compile it for Emacs 22.
;;
;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.
;;
;; This mode does not yet work with mmm-mode ("multiple major modes" mode),
;; although it could possibly be made to do so with some effort.
;;
;; This code is part of a larger project, in progress, to enable writing
;; Emacs customizations in JavaScript.
;;
;; Please email bug reports and suggestions to the author, or submit them
;; at http://code.google.com/p/js2-mode/issues

;; TODO:
;;  - set a text prop on autoinserted delimiters and don't biff user-entered ones
;;  - when inserting magic curlies, look for matching close-curly before inserting
;;  - clean up xml member-expr parsing
;;  - add in remaining Ecma strict-mode warnings
;;  - get more use out of the symbol table:
;;    - jump to declaration (put hyperlinks on all non-decl var usages?)
;;    - rename variable/function
;;    - warn on unused var
;;  - add some dabbrev-expansions for built-in keywords like finally, function
;;  - add at least some completion support, e.g. for built-ins
;;  - code formatting

;;; Code:
;;; js2-vars.el -- byte-compiler support for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-langs)    ; it's here in Emacs 21...
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar js2-emacs22 (>= emacs-major-version 22))

(defcustom js2-highlight-level 2
  "Amount of syntax highlighting to perform.
nil, zero or negative means none.
1 adds basic syntax highlighting.
2 adds highlighting of some Ecma built-in properties.
3 adds highlighting of many Ecma built-in functions."
  :type 'integer
  :group 'js2-mode)

(defvar js2-mode-dev-mode-p nil
  "Non-nil if running in development mode.  Normally nil.")

(defgroup js2-mode nil
  "An improved JavaScript mode."
  :group 'languages)

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              2)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :type 'integer)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-cleanup-whitespace t
  "Non-nil to invoke `delete-trailing-whitespace' before saves."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste etc. can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-mirror-mode t
  "Non-nil to insert closing brackets, parens, etc. automatically."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-bounce-indent-flag t
  "Non-nil to have indent-line function choose among alternatives.
If nil, the indent-line function will indent to a predetermined column
based on heuristic guessing.  If non-nil, then if the current line is
already indented to that predetermined column, indenting will choose
another likely column and indent to that spot.  Repeated invocation of
the indent-line function will cycle among the computed alternatives.
See the function `js2-bounce-indent' for details."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-indent-on-enter-key nil
  "Non-nil to have Enter/Return key indent the line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-enter-indents-newline t
  "Non-nil to have Enter/Return key indent the newly-inserted line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-rebind-eol-bol-keys t
  "Non-nil to rebind beginning-of-line and end-of-line keys.
If non-nil, bounce between bol/eol and first/last non-whitespace char."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-electric-keys '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
  "Keys that auto-indent when `js2-auto-indent-flag' is non-nil.
Each value in the list is passed to `define-key'."
  :type 'list
  :group 'js2-mode)

(defcustom js2-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js2-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js2-mode)
(make-variable-buffer-local 'js2-idle-timer-delay)

(defcustom js2-dynamic-idle-timer-adjust 0
  "Positive to adjust `js2-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js2-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js2-idle-timer-delay is multiplied by 2.
If `js2-dynamic-idle-timer-adjust' is 0 or negative,
`js2-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js2-mode)

(defcustom js2-mode-escape-quotes t
  "Non-nil to disable automatic quote-escaping inside strings."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-squeeze-spaces t
  "Non-nil to normalize whitespace when filling in comments.
Multiple runs of spaces are converted to a single space."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-parse-errors t
  "True to highlight parse errors."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262 forbids them, but many browsers permit them.  IE is the
big exception, and can produce bugs if you have trailing commas."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-missing-semi-warning t
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-basic-offset c-basic-offset
  "Functions like `c-basic-offset' in js2-mode buffers."
  :type 'integer
  :group 'js2-mode)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-language-version 170
  "Configures what JavaScript language version to recognize.
Currently only 150, 160 and 170 are supported, corresponding
to JavaScript 1.5, 1.6 and 1.7, respectively.  In a nutshell,
1.6 adds E4X support, and 1.7 adds let, yield, and Array
comprehensions."
  :type 'integer
  :group 'js2-mode)

(defcustom js2-allow-keywords-as-property-names nil
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

  var foo = {int: 5, while: 6, continue: 7};
  foo.return = 8;

Ecma-262 forbids this syntax, but many browsers support it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-rhino-new-expr-initializer nil
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

  new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

  new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

NOTE:  this is currently a placeholder, and `js2-mode' does not yet
support this syntax.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

  function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

  function a.b().c[2] (x, y) { ... }

is rewritten as:

  a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-version 20080521
  "Release number for `js2-mode'.")

;; scanner variables

;; We record the start and end position of each token.
(defvar js2-token-beg 1)
(make-variable-buffer-local 'js2-token-beg)
(defvar js2-token-end -1)
(make-variable-buffer-local 'js2-token-end)

(defvar js2-EOF_CHAR -1
  "Represents end of stream.  Distinct from js2-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js2-' prefix in front of each name.
(defvar js2-ERROR -1)
(defvar js2-EOF 0)
(defvar js2-EOL 1)
(defvar js2-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js2-LEAVEWITH 3)
(defvar js2-RETURN 4)
(defvar js2-GOTO 5)
(defvar js2-IFEQ 6)
(defvar js2-IFNE 7)
(defvar js2-SETNAME 8)
(defvar js2-BITOR 9)
(defvar js2-BITXOR 10)
(defvar js2-BITAND 11)
(defvar js2-EQ 12)
(defvar js2-NE 13)
(defvar js2-LT 14)
(defvar js2-LE 15)
(defvar js2-GT 16)
(defvar js2-GE 17)
(defvar js2-LSH 18)
(defvar js2-RSH 19)
(defvar js2-URSH 20)
(defvar js2-ADD 21)            ; infix plus
(defvar js2-SUB 22)            ; infix minus
(defvar js2-MUL 23)
(defvar js2-DIV 24)
(defvar js2-MOD 25)
(defvar js2-NOT 26)
(defvar js2-BITNOT 27)
(defvar js2-POS 28)            ; unary plus
(defvar js2-NEG 29)            ; unary minus
(defvar js2-NEW 30)
(defvar js2-DELPROP 31)
(defvar js2-TYPEOF 32)
(defvar js2-GETPROP 33)
(defvar js2-GETPROPNOWARN 34)
(defvar js2-SETPROP 35)
(defvar js2-GETELEM 36)
(defvar js2-SETELEM 37)
(defvar js2-CALL 38)
(defvar js2-NAME 39)           ; an identifier
(defvar js2-NUMBER 40)
(defvar js2-STRING 41)
(defvar js2-NULL 42)
(defvar js2-THIS 43)
(defvar js2-FALSE 44)
(defvar js2-TRUE 45)
(defvar js2-SHEQ 46)           ; shallow equality (===)
(defvar js2-SHNE 47)           ; shallow inequality (!==)
(defvar js2-REGEXP 48)
(defvar js2-BINDNAME 49)
(defvar js2-THROW 50)
(defvar js2-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js2-IN 52)
(defvar js2-INSTANCEOF 53)
(defvar js2-LOCAL_LOAD 54)
(defvar js2-GETVAR 55)
(defvar js2-SETVAR 56)
(defvar js2-CATCH_SCOPE 57)
(defvar js2-ENUM_INIT_KEYS 58)
(defvar js2-ENUM_INIT_VALUES 59)
(defvar js2-ENUM_INIT_ARRAY 60)
(defvar js2-ENUM_NEXT 61)
(defvar js2-ENUM_ID 62)
(defvar js2-THISFN 63)
(defvar js2-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js2-ARRAYLIT 65)       ; array literal
(defvar js2-OBJECTLIT 66)      ; object literal
(defvar js2-GET_REF 67)        ; *reference
(defvar js2-SET_REF 68)        ; *reference = something
(defvar js2-DEL_REF 69)        ; delete reference
(defvar js2-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js2-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js2-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; XML support
(defvar js2-DEFAULTNAMESPACE 73)
(defvar js2-ESCXMLATTR 74)
(defvar js2-ESCXMLTEXT 75)
(defvar js2-REF_MEMBER 76)     ; Reference for x.@y, x..y etc.
(defvar js2-REF_NS_MEMBER 77)  ; Reference for x.ns::y, x..ns::y etc.
(defvar js2-REF_NAME 78)       ; Reference for @y, @[y] etc.
(defvar js2-REF_NS_NAME 79)    ; Reference for ns::y, @ns::y@[y] etc.

(defvar js2-first-bytecode js2-ENTERWITH)
(defvar js2-last-bytecode js2-REF_NS_NAME)

(defvar js2-TRY 80)
(defvar js2-SEMI 81)           ; semicolon
(defvar js2-LB 82)             ; left and right brackets
(defvar js2-RB 83)
(defvar js2-LC 84)             ; left and right curly-braces
(defvar js2-RC 85)
(defvar js2-LP 86)             ; left and right parens
(defvar js2-RP 87)
(defvar js2-COMMA 88)          ; comma operator

(defvar js2-ASSIGN 89)         ; simple assignment (=)
(defvar js2-ASSIGN_BITOR 90)   ; |=
(defvar js2-ASSIGN_BITXOR 91)  ; ^=
(defvar js2-ASSIGN_BITAND 92)  ; &=
(defvar js2-ASSIGN_LSH 93)     ; <<=
(defvar js2-ASSIGN_RSH 94)     ; >>=
(defvar js2-ASSIGN_URSH 95)    ; >>>=
(defvar js2-ASSIGN_ADD 96)     ; +=
(defvar js2-ASSIGN_SUB 97)     ; -=
(defvar js2-ASSIGN_MUL 98)     ; *=
(defvar js2-ASSIGN_DIV 99)     ; /=
(defvar js2-ASSIGN_MOD 100)    ; %=

(defvar js2-first-assign js2-ASSIGN)
(defvar js2-last-assign js2-ASSIGN_MOD)

(defvar js2-HOOK 101)          ; conditional (?:)
(defvar js2-COLON 102)
(defvar js2-OR 103)            ; logical or (||)
(defvar js2-AND 104)           ; logical and (&&)
(defvar js2-INC 105)           ; increment/decrement (++ --)
(defvar js2-DEC 106)
(defvar js2-DOT 107)           ; member operator (.)
(defvar js2-FUNCTION 108)      ; function keyword
(defvar js2-EXPORT 109)        ; export keyword
(defvar js2-IMPORT 110)        ; import keyword
(defvar js2-IF 111)            ; if keyword
(defvar js2-ELSE 112)          ; else keyword
(defvar js2-SWITCH 113)        ; switch keyword
(defvar js2-CASE 114)          ; case keyword
(defvar js2-DEFAULT 115)       ; default keyword
(defvar js2-WHILE 116)         ; while keyword
(defvar js2-DO 117)            ; do keyword
(defvar js2-FOR 118)           ; for keyword
(defvar js2-BREAK 119)         ; break keyword
(defvar js2-CONTINUE 120)      ; continue keyword
(defvar js2-VAR 121)           ; var keyword
(defvar js2-WITH 122)          ; with keyword
(defvar js2-CATCH 123)         ; catch keyword
(defvar js2-FINALLY 124)       ; finally keyword
(defvar js2-VOID 125)          ; void keyword
(defvar js2-RESERVED 126)      ; reserved keywords

(defvar js2-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js2-BLOCK 128)         ; statement block
(defvar js2-LABEL 129)         ; label
(defvar js2-TARGET 130)
(defvar js2-LOOP 131)
(defvar js2-EXPR_VOID 132)     ; expression statement in functions
(defvar js2-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js2-JSR 134)
(defvar js2-SCRIPT 135)        ; top-level node for entire script
(defvar js2-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js2-USE_STACK 137)
(defvar js2-SETPROP_OP 138)    ; x.y op= something
(defvar js2-SETELEM_OP 139)    ; x[y] op= something
(defvar js2-LOCAL_BLOCK 140)
(defvar js2-SET_REF_OP 141)    ; *reference op= something

;; For XML support:
(defvar js2-DOTDOT 142)        ; member operator (..)
(defvar js2-COLONCOLON 143)    ; namespace::name
(defvar js2-XML 144)           ; XML type
(defvar js2-DOTQUERY 145)      ; .() -- e.g., x.emps.emp.(name == "terry")
(defvar js2-XMLATTR 146)       ; @
(defvar js2-XMLEND 147)

;; Optimizer-only tokens
(defvar js2-TO_OBJECT 148)
(defvar js2-TO_DOUBLE 149)

(defvar js2-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js2-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js2-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js2-CONST 153)
(defvar js2-SETCONST 154)
(defvar js2-SETCONSTVAR 155)
(defvar js2-ARRAYCOMP 156)
(defvar js2-LETEXPR 157)
(defvar js2-WITHEXPR 158)
(defvar js2-DEBUGGER 159)

(defvar js2-COMMENT 160)  ; not yet in Rhino

(defvar js2-num-tokens (1+ js2-COMMENT))

(defconst js2-debug-print-trees nil)

;; Rhino accepts any string or stream as input.
;; Emacs character processing works best in buffers, so we'll
;; assume the input is a buffer.  JavaScript strings can be
;; copied into temp buffers before scanning them.

(defmacro deflocal (name value comment)
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(deflocal js2-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(deflocal js2-ts-regexp-flags nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-string ""
  "Token stream buffer-local variable.
Last string scanned.")

(deflocal js2-ts-number nil
  "Token stream buffer-local variable.
Last literal number scanned.")

(deflocal js2-ts-hit-eof nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-start 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-lineno 1
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-end-char -1
  "Token stream buffer-local variable.")

(deflocal js2-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(deflocal js2-ts-is-xml-attribute nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-is-tag-content nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-open-tags-count 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(deflocal js2-ts-comment-type nil
  "Token stream buffer-local variable.")

;;; Parser variables

(defvar js2-parsed-errors nil
  "List of errors produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-errors)

(defvar js2-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-warnings)

(defvar j
