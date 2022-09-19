;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl)
  (:export #:with-html #:with-html-string #:html
           #:*html*
           #:*html-lang* #:*html-charset*
           #:get-html-path
           #:do-elements
           #:deftag
           #:*unvalidated-attribute-prefixes*
           #:*boolean-attributes*
           #:*fill-column*
           #:html-length
           #:dynamic-tag
           #:*html-style*
           #:spinneret-error
           #:no-such-tag
           #:*suppress-inserted-spaces*
           #:interpret-html-tree)
  (:import-from #:parenscript
                #:concat-constant-strings ;; unexported function
                #:define-ps-symbol-macro
                #:defpsmacro
                #:with-ps-gensyms)
  (:import-from #:trivial-gray-streams
                #:fundamental-character-output-stream
                #:stream-write-char #:stream-write-string
                #:stream-terpri
                #:stream-fresh-line
                #:stream-finish-output
                #:stream-force-output
                #:stream-advance-to-column
                #:stream-start-line-p)
  (:import-from #:alexandria
                #:array-index
                #:clamp
                #:string-designator
                #:make-keyword
                #:parse-body #:parse-ordinary-lambda-list
                #:with-gensyms #:with-unique-names
                #:remove-from-plist
                #:starts-with-subseq
                #:when-let #:if-let
                #:assoc-value
                #:disjoin
                #:doplist
                #:once-only)
  (:import-from #:serapeum
                #:fmt #:eif #:econd
                #:define-do-macro #:defconst
                #:nlet #:nix #:assure
                #:find-keyword
                #:-> #:with-thunk
                #:and-let* #:op #:string-prefix-p
                #:memq
                #:string$=
                #:string^=
                #:escape
                #:defconst
                #:defconstructor
                #:string-replace-all
                #:local*
                #:fbind
                #:fbind*
                #:bound-value
                #:defmethods
                #:eval-if-constant
                #:parse-leading-keywords)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:trivia
                #:match)
  (:import-from #:global-vars
                #:define-global-parameter))

(defpackage #:spinneret-user
  (:use #:cl #:parenscript #:spinneret))
