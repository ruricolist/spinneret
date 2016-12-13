# Spinneret

In the crowded space of Common Lisp HTML generators, SPINNERET
occupies the following coordinates:

- Modern. Targets HTML5. Does not treat XML and HTML as the same
  problem. Assumes you will be serving your documents as UTF-8.

- Composable. Makes it easy to refactor HTML generation into separate
  functions and macros.

- Pretty. Treats HTML as a document format, not a serialization.
  Output is idiomatic and readable, following the coding style of the
  HTML5 specification.

- Aggressive. If something can be interpreted as HTML, then it will
  be, meaning that some Lisp forms can't be mixed with HTML syntax. In
  the trade-off between 90% convenience and 10% correctness SPINNERET
  is on the side of convenience.

- Bilingual. Spinneret has the same semantics in Lisp and Parenscript.

HTML generation with SPINNERET looks like this:

     (in-package #:spinneret)

     (defparameter *shopping-list*
       '("Atmospheric ponds"
         "Electric gumption socks"
         "Mrs. Leland's embyronic television combustion"
         "Savage gymnatic aggressors"
         "Pharmaceutical pianos"
         "Intravenous retribution champions"))

     (defparameter *user-name* "John Q. Lisper")

     (defparameter *last-login* "12th Never")

     (defmacro with-page ((&key title) &body body)
       `(with-html
          (:doctype)
          (:html
            (:head
             (:title ,title))
            (:body ,@body))))

     (defun shopping-list ()
       (with-page (:title "Home page")
         (:header
          (:h1 "Home page"))
         (:section
          ("~A, here is *your* shopping list: " *user-name*)
          (:ol (dolist (item *shopping-list*)
                 (:li (1+ (random 10)) item))))
         (:footer ("Last login: ~A" *last-login*))))

Which produces:

     <!DOCTYPE html>
     <html lang=en>
      <head>
       <meta charset=UTF-8>
       <title>Home page</title>
      </head>
      <body>
       <header>
        <h1>Home page</h1>
       </header>
       <section>
        John Q. Lisper, here is <em>your</em> shopping list:
        <ol>
         <li>10 Atmospheric ponds
         <li>6 Electric gumption socks
         <li>4 Mrs. Leland's embyronic television combustion
         <li>9 Savage gymnatic aggressors
         <li>6 Pharmaceutical pianos
         <li>9 Intravenous retribution champions
        </ol>
       </section>
       <footer>
        Last login: 12th Never
       </footer>
      </body>
     </html>

(Pretty-printing is pretty fast, but SPINNERET obeys `*print-pretty*`
should you want to turn it off.)

## Syntax

The rules for WITH-HTML are these:

- All generated forms write to `*html*`.

- A keyword in function position is interpreted as a tag name. If the
  name is not valid as a tag, it is ignored.

  Certain keywords are recognized as pseudo-tags and given special
  treatment:

  :RAW :DOCTYPE :!DOCTYPE :CDATA :!-- :COMMENT :HTML :HEAD

  The value of the LANG attribute of HTML is controlled by
  `*html-lang*`; the value of the meta charset attribute is controlled
  by `*html-charset*`.

  Constant classes and ids can be specified with a selector-like
  syntax. E.g.:

     (:div#wrapper (:div.section ...))
     ≡ (:div :id "wrapper" (:div :class "section" ...))

- Keyword-value pairs following a tag are interpreted as attributes.
  HTML syntax may not be used in attribute values. Attributes with nil
  values are omitted from the output. Boolean attributes with non-nil
  values are minimized.

  Duplicate attributes are handled like duplicate keyword arguments:
  all values are evaluated, but only the leftmost value is used. The
  exception is the handling of tokenized attributes, such as :CLASS or
  :REL. The class of a tag is the union of all its :CLASS arguments.

  The argument :DATASET introduces a list of :DATA-FOO arguments:

     (:p :dataset (:duck (dolomphious) :fish 'fizzgigious
                         :spoon "runcible"))
     ≡ (:p :data-duck (dolomphious) :data-fish 'fizzgigious
           :data-spoon "runcible")

  For flexibility, even at the cost of efficiency, the argument :ATTRS
  introduces a form to evaluate at run time for a plist of extra
  attributes and values.

- Forms after the attributes are treated as arguments. Each non-nil
  (primary) value returned by an argument to a tag is written to the
  stream by HTML, a generic function on which you can define your own
  methods. By default only literal arguments are printed. Literal
  arguments are strings, characters, numbers and symbols beside NIL.

WITH-HTML-STRING is like WITH-HTML, but intercepts the generated HTML
at run time and returns a string.

## Markdown

If the additional system `spinneret/cl-markdown` is loaded, then a
string in function position is first compiled as Markdown (using
CL-MARKDOWN), then passed to `format` as a control string and applied
to its arguments.

This is useful for inline formatting, like links, where sexps would be
clumsy:

    (with-html
     ("Here is some copy, with [a link](~a)" link))

    (with-html
      (:span "Here is some copy, with "
        (:a :href link "a link.")))

## `*html-path*`


Sometimes it is useful for a piece of HTML-generating code to know
where in the document it appears. You might, for example, want to
define a `tabulate` that prints list-of-lists as rows of cells, but
only prints the surrounding `<table></table>` if it is not already
within a table. The symbol `*HTML-PATH*` holds a list of open tags, from
latest to earliest. Usually it will look something like

      *html-path* ;-> '(:table :section :body :html)

Thus `tabulate' could be written

     (defun tabulate (&rest rows)
       (with-html
         (flet ((tabulate ()
                  (loop for row in rows do
                    (:tr (loop for cell in row do
                      (:td cell))))))
           (if (find :table *html-path*)
               (tabulate)
               (:table (:tbody (tabulate)))))))

## `deftag`

The stumbling block for all sexp-based HTML generators is order of
evaluation. It's tempting to write something like this:

     ;; Doesn't work
     (defun field (control)
       (with-html (:p control)))

     (defun input (default &key name label (type "text"))
       (with-html
         (:label :for name label)
         (:input :name name :id name :type type :value default)))

But it won't work: in `(field (input "Default" :name "why" :label
"Reason"))`, `(input)` gets evaluated before `(field)`, and the HTML
is printed inside-out.

Macros do work:

     (defmacro field (control)
       `(with-html (:p ,control)))

     (defmacro input (name label &key (type "text"))
       `(with-html
          (:label :for ,name ,label)
          (:input :name ,name :id ,name :type ,type)))

But we can do better than this. Spinneret provides a macro-writing
macro, `deftag`, which lets you *refactor* HTML without *hiding* it.

     (deftag field (control attrs)
      `(:p ,@attrs ,@control))

     (deftag input (default attrs &key name label (type "text"))
       (once-only (name)
         `(progn
            (:label :for ,name ,label)
            (:input :name ,name :id ,name :type ,type
              ,@attrs
              :value (progn ,@default)))))

A macro defined using `deftag` takes its arguments just like an HTML
element. Instead of

    (input "Default" :name "why" :label "Reason") ; defmacro

You write

    (input :name "why" :label "Reason" "Default") ; deftag

The macro re-arranges the arguments so they can be bound to an
ordinary lambda list, like the one above: the body of the tag is bound
to the first argument, and matching attributes are bound to keywords.
Multiple `:class` arguments, `:dataset`, and other shorthands are
handled exactly as in the usual HTML syntax.

But the great advantage of `deftag` is how it handles attributes which
are *not* bound to keywords. In the definition of `input` using
`deftag`, you see that the `attrs` catch-all argument is spliced into
the call to `:input`. This means that any unhandled attributes pass
through to the actual input element.

    (input :name "why" :label "Reason" :required t :class "special" "Default")
    => <label for=why>Reason</label>
       <input class=special name=why id=why type=text required value=Default>

In effect, `input` *extends* the `:input` tag, almost like a subclass.
This is a very idiomatic and expressive way of building abstractions
over HTML.

(SPINNERET used to provide a more elaborate way of building HTML
abstractions, `deftemplate`, but `deftag` is simpler and more useful.)

## Parenscript

The semantics of SPINNERET in Parenscript are almost the same. There
is no `with-html-string`, and `with-html` returns a
`DocumentFragment`.

If Markdown support is enabled, strings in function position are still
parsed as Markdown, but supplying arguments triggers an error (since
Parenscript does not have `format`).

`*html-path*` is not implemented for Parenscript.

## Validation

SPINNERET does not do document validation, but it does warn, at
compile time, about invalid tags and attributes.

Although HTML5 does include a mechanism for application-specific
attributes (the `data-` prefix), some client-side frameworks choose to
employ their own prefixes instead. You can disable validation for a
given prefix by adding it to `*unvalidated-attribute-prefixes*`.

    (pushnew "ng-" *unvalidated-attribute-prefixes* :test #’equal)
