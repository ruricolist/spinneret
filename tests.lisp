(defpackage #:spinneret.tests
  (:use #:cl #:spinneret #:fiveam)
  (:import-from #:alexandria #:ensure-list #:make-keyword)
  (:import-from #:serapeum
                #:~> #:op #:lines #:string-join #:concat #:fmt)
  (:import-from :spinneret :valid-custom-element-name?)
  (:shadow :test)
  (:export #:run-tests))

(in-package #:spinneret.tests)

(def-suite spinneret)
(in-suite spinneret)

(defmacro test (name &body body)
  `(5am:test (,@(ensure-list name) :compile-at :run-time)
     ;; Ensure the expected defaults.
     (let ((*html-style* :human)
           (*print-pretty* t))
       ,@body)))

(defun run-tests ()
  (run! 'spinneret))

(defun visually-equal (string1 string2)
  (let ((lines1 (serapeum:lines string1))
        (lines2 (serapeum:lines string2)))
    (and (= (length lines1)
            (length lines2))
         (every (lambda (line1 line2)
                  (equal (string-right-trim " " line1)
                         (string-right-trim " " line2)))
                lines1 lines2))))

(defun linewise-equal (string1 string2)
  (let ((lines1 (mapcar #'serapeum:trim-whitespace (serapeum:lines string1)))
        (lines2 (mapcar #'serapeum:trim-whitespace (serapeum:lines string2))))
    (and (= (length lines1)
            (length lines2))
         (every #'equal lines1 lines2))))

(defmacro with-pretty-printing (&body body)
  `(let ((*print-pretty* t)
         (*html-style* :human))
     ,@body))

(defmacro without-pretty-printing (&body body)
  `(let ((*print-pretty* nil))
     ,@body))

(test dataset
  (without-pretty-printing
    (flet ((dolomphious () 'dolomphious))
      (is (equal
           "<p data-duck=DOLOMPHIOUS data-fish=FIZZGIGIOUS data-spoon=runcible>"
           (with-html-string
             (:p :dataset (:duck (dolomphious) :fish 'fizzgigious
                           :spoon "runcible"))))))))

(test attrs
  (without-pretty-printing
    (is (equal
         "<p foo=bar baz=quux>bar"
         (let ((attrs '(:foo "bar" :baz "quux")))
           (with-html-string (:p :attrs attrs "bar")))))))

(defun bigtable (&optional (*html* *html*))
  (with-html
    (:table
      (dotimes (i 1000)
        (:tr (dotimes (i 10)
               (:td (1+ i))))))))

(test bigtable
  (flet ((bt (msg)
           (let ((start (get-internal-run-time)))
             (with-output-to-string (*html*)
               (finishes (bigtable)))
             (let* ((end (get-internal-run-time))
                    (duration (- end start))
                    (seconds (/ duration (float internal-time-units-per-second))))
               (format t "~&Bigtable benchmark ~a: ~d second~:p~%" msg seconds)))))
    (let ((*print-pretty* t)
          (*html-style* :human))
      (bt "with pretty printing"))
    (let ((*print-pretty* t)
          (*html-style* :tree))
      (bt "with pretty printing (tree style)"))
    (let ((*print-pretty* nil)
          (*html-style* :human))
      (bt "without pretty printing"))
    (let ((*print-pretty* nil)
          (*html-style* :tree))
      (bt "without pretty printing (tree style)"))))

(defun readme-example ()
  (with-pretty-printing
    (let* ((user-name "John Q. Lisper")
           (last-login "12th Never")
           (shopping-list
             '("Atmospheric ponds"
               "Electric gumption socks"
               "Mrs. Leland's embyronic television combustion"
               "Savage gymnatic aggressors"
               "Pharmaceutical pianos"
               "Intravenous retribution champions"))
           (amounts '(10 6 4 9 6 9)))
      (with-html
        (:doctype)
        (:html
          (:head
            (:title "Home page"))
          (:body
            (:header
              (:h1 "Home page"))
            (:section
              ("~A, here is *your* shopping list: " user-name)
              (:ol (loop for item in shopping-list
                         for amount in amounts
                         do (:li amount item))))
            (:footer ("Last login: ~A" last-login))))))))

(defun readme-example-string ()
  (with-output-to-string (*html*)
    (readme-example)))

(test readme-example
  (with-pretty-printing
    (let* ((expected-string
             (format nil "~
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
    <li>4 Mrs. Leland&#39;s embyronic television combustion
    <li>9 Savage gymnatic aggressors
    <li>6 Pharmaceutical pianos
    <li>9 Intravenous retribution champions
   </ol>
  </section>
  <footer>
   Last login: 12th Never
  </footer>
 </body>
</html>"))
           (*print-pretty* t)
           (generated-string
             (readme-example-string)))
      (is (visually-equal generated-string expected-string)))))

(test indent-problem
  (with-pretty-printing
    (is (visually-equal
         (with-html-string
           (:ul (:li (:a "hai"))))
         (format nil "~
<ul>
 <li><a>hai</a>
</ul>")))

    (is (visually-equal
         (with-html-string
           (:html (:head)
             (:body (:a "hai"))))
         (format nil "~
<html lang=en>
 <head>
  <meta charset=UTF-8>
 </head>
 <body>
  <a>hai</a>
 </body>
</html>")))))

(test space-problem
  (without-pretty-printing
    (is
     (equal
      "<div>hello<a href=#></a> there world</div>"
      (spinneret:with-html-string
        (:div "hello"
          (:a :href "#")
          "there world"))))))

(test explicit-spaces
  (without-pretty-printing
    (is (equal "<div>hi<span> there</span></div>"
               (spinneret:with-html-string (:div "hi" (:span " there"))))))
  (with-pretty-printing
    (is (visually-equal
         #.(format nil "~
<p>hi <span>there</span>")
         (let ((*print-pretty* t))
           (spinneret:with-html-string
             (:p "hi " (:span "there"))))))))

(test null-attr
  (without-pretty-printing
    (is (equal (with-html-string (:li :class nil "Hello"))
               "<li>Hello")))
  (without-pretty-printing
    (is (equal (with-html-string (:li :class nil "Hello"))
               "<li>Hello")))

  (is (equal (with-html-string (:li :class (progn nil)))
             "<li>")))

(test no-final-space-after-skipped-attribute
  (without-pretty-printing
    (is (equal (with-html-string (:a :href "#" :data-instant t))
               "<a href=# data-instant=true></a>"))
    (is (equal (with-html-string (:a :href "#" :data-instant nil))
               "<a href=#></a>"))))

(serapeum:def lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defun lorem-ipsum ()
  (with-pretty-printing
    (let ((*fill-column* 80))
      (with-html
        (:doctype)
        (:html
          (:body
            (:div
              (:p lorem-ipsum (:span)
                (:a :href "" :data-instant t "Hello")
                lorem-ipsum))))))))

(defun lorem-ipsum-string ()
  (with-output-to-string (*html*)
    (lorem-ipsum)))

(test lorem-ipsum
  (is (visually-equal
       #.(format nil
                 "~
<!DOCTYPE html>
<html lang=en>
 <body>
  <div>
   <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
    incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
    nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
    fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
    culpa qui officia deserunt mollit anim id est laborum.<span></span><a href=\"\"
    data-instant=true>Hello</a> Lorem ipsum dolor sit amet, consectetur adipiscing
    elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
    ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
    commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
    esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
    non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  </div>
 </body>
</html>")
       (lorem-ipsum-string))))

(test hello-hello-hello
  (with-pretty-printing
    (is (visually-equal
         "<div>
 <div>
  <div>
   <ul>
    <li><a class=\"class1 class2 class3 class4 class5\" href=\"hello hello hello\"></a>
   </ul>
  </div>
 </div>
</div>"
         (spinneret:with-html-string
           (:div
             (:div
               (:div
                 (:ul
                   (:li
                     (:a.class1.class2.class3.class4.class5
                      :href "hello hello hello")))))))))))

(test inline-element-after-paragraph
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<div>
 <p>Hello
 <a>world</a>
</div>")
         (with-html-string
           (:div
             (:p "Hello")
             (:a "world")))))))

(test indent-attributes-in-blocks
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<input class=form-control type=password
       name=password id=password required>")
         (with-html-string
           (:input :type "password" :name "password"
             :class "form-control" :id "password"
             :required t))))))

(test indent-text-sanely
  (with-pretty-printing
    (is (linewise-equal
         (format nil "~
   <div class=\"last-update col-xs-2 col-md-1\"
        title=\"Last updated 232 days ago\">
    232d
   </div>")
         (with-html-string
           (:div :class "last-update col-xs-2 col-md-1" :title "Last updated 232 days ago"
             "232d"))))))

(defun indent-string (string n)
  "Add N spaces at the beginning of each line of STRING."
  (let ((padding (make-string n :initial-element #\Space)))
    (~> string
        lines
        (mapcar (op (concat padding _)) _)
        (string-join #\Newline))))

(test indent-sanely-in-blocks-in-paragraphs
  (with-pretty-printing
    (is (serapeum:string*=
         (indent-string
          (with-html-string
            (:div :class "status col-xs-2 col-md-1"
              (:span :class "text-success"
                (:a :href "https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#200"
                  200))))
          ;; Stick an extra space on each line.
          1)
         (with-html-string
           (:li
             (:div :class "status col-xs-2 col-md-1"
               (:span :class "text-success"
                 (:a :href "https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#200"
                   200)))))))))

;; (test (indent-closing-inline-tags-in-blocks :compile-at :run-time)
;;   (let ((*print-pretty* t))
;;     (is (visually-equal
;;          (format nil "~
;; <div>
;;  <span>
;;   <a href=#>Hello</a>
;;  </span>
;; </div>")
;;          (with-html-string
;;            (:div
;;              (:span
;;                (:a :href "#" "Hello"))))))))

(test indent-inline-after-paragraph
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<p>
 <button>Log in</button>
 <a href=#>Forgot?</a>")
         (with-html-string
           (:p
             (:button "Log in")
             (:a :href "#" "Forgot?")))))))

(test empty-tags-on-same-line
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<div>
 <div></div>
</div>")
         (with-html-string
           (:div
             (:div)))))))

(test misaligned-attrs-in-nested-blocks
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<div>
 <div>
  <div>
   <div class=\"list-group toc-entries\"
        data-instant=true></div>
  </div>
 </div>
</div>")
         (with-html-string
           (:div
             (:div
               (:div
                 (:div.list-group.toc-entries
                  :data-instant t)))))))))

(test keywords-in-tokenized-attributes
  (with-pretty-printing
    (is (equal "<p class=foo>"
               (with-html-string
                 (:p :class :foo))))
    (is (equal "<link rel=stylesheet>"
               (with-html-string
                 (:link :rel :stylesheet))))))

(test dynamic-tags
  (with-pretty-printing
    (is (visually-equal
         (with-html-string
           (:div
             (:section
               (:h2
                 (:p "hello")))))
         (with-html-string
           (:div
             (:section
               (:tag :name :h2
                     (:p "hello")))))))))

(test h*
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<body>
 <h1>This is a top level heading</h1>
 <p>...
 <section>
  <p>...
  <h2>This is a second-level heading</h2>
  <p>...
  <h2>This is another second-level heading</h2>
  <p>...
  <section>
   <h3>This is a third-level heading</h3>
   <p>...
  </section>
 </section>
</body>")
         (with-html-string
           (:body
             (:h* "This is a top level heading")
             (:p "...")
             (:section
               (:p "...")
               (:h* "This is a second-level heading")
               (:p "...")
               (:h* "This is another second-level heading")
               (:p "...")
               (:section
                 (:h* "This is a third-level heading")
                 (:p "...")))))))))

(test html-path
  (is (visually-equal
       (format nil "~
<body>
 <h1>This is a top level heading</h1>
 <p>...
 <h2>This is a second-level tricked by *HTML-PATH*</h2>
 <p>...
</body>")
       (with-html-string
         (:body
           (:h* "This is a top level heading")
           (:p "...")
           (let ((*html-path* (append *html-path* '(:section))))
             (:h* "This is a second-level tricked by *HTML-PATH*")
             (:p "...")))))))


(test print-tree
  (with-pretty-printing
    (is (visually-equal
         (format nil "~
<div>
 <p>Text <a>link text</a> more text
</div>")
         (let ((*html-style* :human))
           (with-html-string
             (:div
               (:p "Text " (:a "link text") " more text"))))))

    (is (visually-equal
         (format nil "~
<div>
 <p>
  Text
  <a>
   link text
  </a>
   more text
 </p>
</div>")
         (let ((*html-style* :tree))
           (with-html-string
             (:div
               (:p "Text " (:a "link text") " more text"))))))))

(test textarea-preformatting
  (flet ((test1 ()
           (with-html-string
             (:div (:textarea "123"))))
         (test2 ()
           (with-html-string
             (let ((*print-pretty*))
               (:div (:textarea "123"))))))
    (with-pretty-printing
      (is (visually-equal (test1)
                          (format nil "~
<div>
 <textarea>123</textarea>
</div>")))
      (is (visually-equal (test2)
                          "<div><textarea>123</textarea></div>")))
    ;; Test that dereferencing the underlying stream works when the
    ;; stream is not, in fact, an HTML stream.
    (without-pretty-printing
      (is (visually-equal (test2)
                          "<div><textarea>123</textarea></div>")))))

(test print-as-tree-without-pretty-printing
  (is (visually-equal "<p>hello</p><span>world</span>"
                      (let ((spinneret:*html-style* :tree)
                            (*print-pretty* nil))
                        (spinneret:with-html-string
                          (:p "hello")
                          (:span "world"))))))


(test raw-shouldnt-pretty-print-its-content
  (is (visually-equal
       "Very very very very very very very very very very very very very very very very very very very very very very very very long line"
       (with-html-string
         (:raw "Very very very very very very very very very very very very very very very very very very very very very very very very long line")))))

(test valid-custom-element-names
  (is (not (valid-custom-element-name? :x)))
  (is (not (valid-custom-element-name? :-)))
  (is (not (valid-custom-element-name? :-a)))
  (is (valid-custom-element-name? :a-))
  (is (not (valid-custom-element-name? (make-keyword "a")))))

(test literal-custom-element-names
  (signals error
    (eval
     '(with-html-string
       (:xy "Hello"))))

  (finishes
    (eval
     '(with-html-string
       (:x-y "Hello")))))

(test dynamic-custom-element-names
  (signals error
    (eval
     '(with-html-string
       (:tag :name "xy" "Hello"))))
  (finishes
    (eval
     '(with-html-string
       (:tag :name "x-y" "Hello")))))

(test function-space-discrepancy
  (let ((*html-style* :human)
        (*print-pretty* t))
    (is (equal
         (with-html-string
           (:p "foo"
             "bar"
             (values "baz")))
         (with-html-string
           (:p (values "foo")
             "bar"
             "baz"))))
    (is (visually-equal
         (with-html-string
           (:p "foo"
             "bar"
             "baz"))
         (with-html-string
           (:p (values "foo")
             (values "bar")
             (values "baz")))))))

(test literal-pathnames
  (finishes
    (with-html-string
      (:html
        (:head
          (:link #p"styles.css" :type "text/css"))))))

(test pre-closing-tag
  (is
   (visually-equal
    "<div class=some-class>
 <pre>verbatim line one
verbatim line two</pre>
 <p>Some following stuff
</div>"
    (with-html-string
      (:div.some-class
       (:pre "verbatim line one
verbatim line two")
       (:p "Some following stuff")))))
  )

(test pre-no-spaces
  (is
   (visually-equal
    "<pre>foo
bar</pre>"
    (with-html-string (:pre "foo" #\Newline "b" "a" "r")))))

(test pre-no-spaces-format
  (is
   (visually-equal
    "<pre>foo
bar</pre>"
    (with-html-string (:pre ("foo ~A~A~A~A" #\Newline "b" "a" "r"))))))

(test pre-code
  (let ((*print-pretty* t))
    (is
     (visually-equal
      #.(format nil "~
<pre><code>(defun blah ()
    (+ 1 2))</code></pre>")
      (spinneret:with-html-string
        (:pre
          (:code "(defun blah ()
    (+ 1 2))")))))))

(test heading-depth
  (let ((*print-pretty* nil))
    (is (search "h1" (spinneret:with-html-string (:h*))))
    (is (search "h2" (spinneret:with-html-string (:section (:h*)))))))

(test ps-attributes
  (is (not (search "classvar()"
                   (ps:ps
                     (let ((classvar "myclass"))
                       (spinneret:with-html
                         (:div#myid :class classvar
                                    (:p "lorem ipsum")))))))))

(test double-cdata-close
  (is (equal (with-html-string
               (:html
                 (:head
                   (:script
                     (:CDATA "foo")))))
             "<html lang=en>
 <head>
  <meta charset=UTF-8>
  <script><![CDATA[foo]]></script>
 </head>
</html>")))

(test double-comment-close
  (is (equal
       (let (*print-pretty*)
         (with-html-string
           (:html
             (:!-- "something"))))
       "<html lang=en><!-- something --></html>")))

(test interpret-tree
  (is (visually-equal
       (with-output-to-string (*html*)
         (interpret-html-tree
          `(:ul :class "shuffle" (:li "Item1") (:li "Item2"))
          :stream *html*))
       (fmt
        "~
<ul class=shuffle>
 <li>
  Item1
 </li>
 <li>
  Item2
 </li>
</ul>"))))

(test escape-quotes
  (is (equal (with-html-string
               (:p "She said, \"'Hello', she said.\""))
             "<p>She said, &quot;&#39;Hello&#39;, she said.&quot;")))

(test escape-single-quotes-in-attributes
  (is (equal
       (let (*print-pretty*)
         (with-html-string
           (:button :onclick "window.alert('Hello, world.')"  "My button")))
       ;; Interestingly this still works.
       "<button onclick=\"window.alert(&#39;Hello, world.&#39;)\">My button</button>")))

(test raw-metatag
  (is (equal "ahahaha"
             (with-output-to-string (*html*)
               (interpret-html-tree '(:raw "ahahaha")))))
  (is (search "lang=en"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:html (:p "Hello"))))))
  (is (search "!DOCTYPE"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:html (:doctype))))))
  (is (search "ahahaha -->"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:comment "ahahaha")))))
  (is (search "charset=UTF-8"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:html (:head))))))
  (is (equal "<dangerous & forbidden>"
             (with-output-to-string (*html*)
               (interpret-html-tree '(:raw "<dangerous & forbidden>")))))
  (is (search "h3"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:h* (:section (:h* (:section (:h*)))))))
              ))
  (is (search "<p>"
              (with-output-to-string (*html*)
                (interpret-html-tree '(:tag :name :p))))))

(test dissect-interpreted-tag
  (let ((spinneret:*html-style* :tree))
    (is (equal
         (with-html-string
           (:div.my-class))
         (remove #\Newline
                 (with-output-to-string (*html*)
                   (interpret-html-tree '(:div.my-class))))))))

(test dissect-dynamic-tag
  (let ((spinneret:*html-style* :tree))
    (is (equal
         (with-html-string
           (:tag :name :div.my-class))
         (remove #\Newline
                 (with-output-to-string (*html*)
                   (interpret-html-tree '(:div.my-class))))))))

(test override-lang-defaults
  (let ((string
          (with-html-string
            (:html
              (:head)
              (:p)))))
    (is (search "lang=en" string))
    (is (search "charset=UTF-8" string)))
  (let ((string
          (with-html-string
            (:html :lang "foo"
              (:head
                (:meta :charset "some-other"))
              (:p)))))
    (is (search "lang=foo" string))
    (is (search "charset=some-other" string))))

(test allow-allowfullscreen
  (finishes
    (let ((*html* (make-broadcast-stream)))
      (with-html (:iframe :width 560 :height 315 :src "example.com" :title "YouTube video player" :frameborder 0 :allow "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" :allowfullscreen t)))))

(test always-quote-attributes
  (let ((spinneret:*html-style* :tree)
	(spinneret:*always-quote* t))
    (is (equal
         (with-html-string
           (:img :attrs (list :alt "some alt text" :src "https://test.com/image.png")))
         "<img alt=\"some alt text\"
     src=\"https://test.com/image.png\">"))))

(test raw-attributes
  (is (equal
       "<div onclick=\"alert('Hello');\"></div>"
       (with-html-string
         (:div :onclick (:raw (ps:ps (alert "Hello")))))))
  (is (equal
       (with-html-string
         (:div :onclick (ps:ps (alert "Hello"))))
       "<div onclick=alert(&#39;Hello&#39;);></div>")))

(deftag ul* (body attrs &key &allow-other-keys)
  "<ul> with every form (except <li>) in BODY auto-wrapped into a <li>."
  `(:ul ,@attrs ,@(loop for form in body
                        when (and (listp form)
                                  (eq :li (first form)))
                          collect form
                        else
                          collect `(:li ,form))))

(test with-html-over-deftag
  ;; The tag is bound as a macro.
  (is (fboundp 'ul*))
  (is (equal
       (with-html-string
         (ul*
           "Item 1"
           "Item 2"
           (:b "Bold item 3")
           (:li "Proper <li> item 4")
           "Item 5"))
       "<ul>
 <li>Item 1
 <li>Item 2
 <li><b>Bold item 3</b>
 <li>Proper &lt;li&gt; item 4
 <li>Item 5
</ul>")))

(deftag :selfref (body attrs &key href &allow-other-keys)
  `(:a.selfref :href ,href ,@attrs ,@body))

(test deftag-selector-syntax
  ;; The tag is not bound as a macro.
  (is (not (fboundp :selfref)))
  ;; The tag works.
  (is (equal
       "<a class=selfref href=\"https://example.com\" id=id>Example website</a>"
       (with-html-string
         (:selfref#id :href "https://example.com" "Example website")))))

(test inline-tag-leading-spaces
  (flet ((f (url)
           (with-html
             (:h4 (:raw "&nbsp;") (:a :href url "Some Text")))))
    (is (equal
         "<h4>&nbsp;<a href=\"http://short.com/\">Some Text</a></h4>"
         (with-html-string
           (f "http://short.com/"))))
    (is (search ">Some"
                (with-html-string
                  (f "http://thisisreallyreallylonglonglonglongonwegoxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.com"))))))

(test inline-tag-trailing-spaces
  (is (equal
       (spinneret:with-html-string
         (:span "Click the " (:a :href "https://google.com" ) "."))
       "<span>Click the <a href=\"https://google.com\"></a>.</span>")))

(test html-tag-empty-string
  (finishes
    (spinneret:with-html-string (:style ""))))

(test test-dataset-property-within-hyphens
  "Hyphenated data properties should be translated to bracketed strings
with underscores."
  (is (search
       "dataset['x_y']"
       (ps:ps (with-html (:div :data-x-y "z"))))))
