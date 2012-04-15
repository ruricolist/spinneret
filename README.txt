In the crowded space of Common Lisp HTML generators, SPINNERET
occupies the following coordinates:

- Modern. Targets HTML5. Does not treat XML and HTML as the same
  problem. Assumes you will be serving your documents as UTF-8.

- Flexible. Does not restrict tags: HTML5 is meant to be a living
  standard.

- Composable. Makes it easy to refactor HTML generation into separate
  functions and macros.

- Pretty. Treats HTML as a document format, not a serialization.
  Output is idiomatic and readable (follows the coding style of the
  HTML5 specification).

- Aggressive. If something can be interpreted as HTML, then it will
  be, meaning that some Lisp forms can't be mixed with HTML syntax. In
  the trade-off between 90% convenience and 10% correctness SPINNERET
  is on the side of convenience.

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

The rules for WITH-HTML are these:

- All generated forms write to *HTML*.

- A keyword in function position is interpreted as a tag name.

  Certain keywords are recognized as pseudo-tags and given special
  treatment:

  :DOCTYPE :!DOCTYPE :CDATA :!-- :COMMENT :HTML :HEAD

  The value of the LANG attribute of HTML is controlled by
  *HTML-LANG*; the value of the meta charset attribute is controlled
  by *HTML-CHARSET*.

- Keyword-value pairs following a tag are interpreted as attributes.
  HTML syntax may not be used in attribute values. Attributes with nil
  values are omitted. Boolean attributes with non-nil values are
  minimized.

  Duplicate attributes are handled like duplicate keyword arguments:
  all values are evaluated, but only the leftmost value is used. The
  exception is the class attribute: the class of a tag is the union of
  all its :CLASS arguments.

- Forms after the attributes are treated as arguments. Each non-nil
  (primary) value returned by an argument to a tag is written out
  using PRINC.

- A string in function position is first compiled as Markdown (using
  CL-MARKDOWN), then passed to FORMAT as a control string and applied
  to its arguments.

WITH-HTML-STRING is like WITH-HTML, but intercepts the generated HTML
at run time and returns a string.

SPINNERET is tolerant by design; it does not check whether keywords
designate valid tags. Given the inevitability of typos, however,
warnings about invalid tags are available at run time by binding
*CHECK-TAGS* to a non-nil value. (MathML and SVG tags are never
checked.)

So far integration with CL-MARKDOWN is crude, because SPINNERET is not
aware of the structure of the generated HTML and treats it as a run
of text. This may change in the future.

Depends on TRIVIAL-GARBAGE and CL-MARKDOWN, which are
Quicklisp-installable.
