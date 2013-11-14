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

(Pretty-printing is pretty fast, but SPINNERET obeys *print-pretty*
should you want to turn it off.)

The rules for WITH-HTML are these:

- All generated forms write to *HTML*.

- A keyword in function position is interpreted as a tag name. If the
  name is not valid as a tag, it is ignored.

  Certain keywords are recognized as pseudo-tags and given special
  treatment:

  :RAW :DOCTYPE :!DOCTYPE :CDATA :!-- :COMMENT :HTML :HEAD

  The value of the LANG attribute of HTML is controlled by
  *HTML-LANG*; the value of the meta charset attribute is controlled
  by *HTML-CHARSET*.

  Constant classes and ids can be specified with a selector-like
  syntax. E.g.:

     (:div#wrapper (:div.section ...))
     ≡ (:div :id "wrapper" (:div :class "section" ...))

- Keyword-value pairs following a tag are interpreted as attributes.
  HTML syntax may not be used in attribute values. Attributes with nil
  values are omitted. Boolean attributes with non-nil values are
  minimized.

  Duplicate attributes are handled like duplicate keyword arguments:
  all values are evaluated, but only the leftmost value is used. The
  exception is the class attribute: the class of a tag is the union of
  all its :CLASS arguments.

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

- A string in function position is first compiled as Markdown (using
  CL-MARKDOWN), then passed to FORMAT as a control string and applied
  to its arguments.

WITH-HTML-STRING is like WITH-HTML, but intercepts the generated HTML
at run time and returns a string.


Sometimes it is useful for a piece of HTML-generating code to know
where in the document it appears. You might, for example, want to
define a `tabulate' that prints list-of-lists as rows of cells, but
only prints the surrounding <table></table> if it is not already
within a table. The symbol *HTML-PATH* holds a list of open tags, from
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


The stumbling block for all sexp-based HTML generators is order of
evaluation. It's tempting to write something like this:

     ;; Doesn't work
     (defun field (control)
       (with-html (:p control)))

     (defun input (name label &key (type "text"))
       (with-html
         (:label :for name label)
         (:input :name name :id name :type type)))

But it won't work: in (field (input "why" "Reason")), (input) gets
evaluated before (field), and the HTML is printed inside-out. Macros
do work:

     (defmacro field (control)
       `(with-html (:p ,control)))

     (defmacro input (name label &key (type "text"))
       `(with-html
          (:label :for ,name ,label)
          (:input :name ,name :id ,name :type ,type))),

But macros are clumsy instruments for refactoring. Whenever they
change, all their callers have to be recompiled, which hobbles
incremental development.

For these reasons, SPINNERET provides templates. With templates, you
can approximate the obvious solution.

     (deftemplate field (control)
       (:p control))

     (deftemplate input (name label &key (type "text"))
       (:label :for name label)
       (:input :name name :id name :type type))

Templates do not need backquoting and do not require safeguards
against multiple evaluation. Changes to the definition of a template
are immediately visible to its callers. (Templates are still, however,
macros, not functions.)

By default, templates treat the &rest parameter like any other and
splice it in place. To iterate over it instead, use DO-ELEMENTS. The
syntax is the same as DOLIST.

     (deftemplate ul (&rest items)
       (:ul (do-elements (item items)
              (:li item))))

For such simple uses, templates are overkill. There are two prominent
drawbacks to templates: they are not first-class (cannot be used with
FUNCALL or APPLY), and optional and keyword arguments only allow
constant initforms.

Their intended purpose is to abstract the boilerplate required by CSS
frameworks. For example, using Bootstrap, you could define an
abstraction over alerts like so:

   (deftemplate alert (body &key (type :info) (dismissable t))
     (with-html
       (:div.alert
        :class (ecase type
                 (:info "alert-info")
                 (:success "alert-success")
                 (:warning "alert-warning")
                 (:danger "alert-danger"))
        :class (when dismissable
                 "alert-dismissable")
        (when dismissable
          (:button.close :type :button :data-dismiss t :aria-hidden t))
        body)))

To use it:

     (alert
       (:p (:strong "Well done!")
         ("You successfully read [this important alert message](~a)."
           "http://example.com")))


So far integration with CL-MARKDOWN is crude, because SPINNERET is not
aware of the structure of the generated HTML and treats it as a run of
text. This may change in the future.


The semantics of SPINNERET in Parenscript are almost the same. There
is no WITH-HTML-STRING, and WITH-HTML returns a DocumentFragment.
Strings in function position are still parsed as Markdown, but
supplying arguments triggers an error (since Parenscript does not have
FORMAT). Templates and *HTML-PATH* are not implemented for
Parenscript.


SPINNERET does not do document validation, but it does warn, at
compile time, about invalid tags and attributes.


Depends on TRIVIAL-GARBAGE, CL-MARKDOWN, PARENSCRIPT and ALEXANDRIA,
which are Quicklisp-installable.
