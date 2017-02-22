(defpackage #:spinneret.tests
  (:use #:cl #:spinneret #:fiveam)
  (:export #:run-tests))

(in-package #:spinneret.tests)

(def-suite spinneret)
(in-suite spinneret)

(defun run-tests ()
  (run! 'spinneret))

(defun equal-sans-trailing-spaces (string1 string2)
  (let* ((lines1 (serapeum:lines string1))
         (lines2 (serapeum:lines string2)))
    (and (= (length lines1)
            (length lines2))
         (every (lambda (line1 line2)
                  (equal (string-right-trim " " line1)
                         (string-right-trim " " line2)))
                lines1 lines2))))

(test dataset
  (let (*print-pretty*)
    (flet ((dolomphious () 'dolomphious))
      (is (equal
           "<p data-duck=DOLOMPHIOUS data-fish=FIZZGIGIOUS data-spoon=runcible>"
           (with-html-string
             (:p :dataset (:duck (dolomphious) :fish 'fizzgigious
                           :spoon "runcible"))))))))

(test attrs
  (let (*print-pretty*)
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
  (flet ((bt (pp)
           (let ((*print-pretty* pp))
             (let ((start (get-internal-run-time)))
               (with-output-to-string (*html*)
                 (finishes (bigtable)))
               (let* ((end (get-internal-run-time))
                      (duration (- end start))
                      (seconds (/ duration (float internal-time-units-per-second))))
                 (format t "~&Bigtable benchmark ~:[without~;with~] pretty printing: ~d second~:p~%"
                         pp seconds))))))
    (bt t)
    (bt nil)))

(test readme-example
  (let* ((user-name "John Q. Lisper")
         (last-login "12th Never")
         (shopping-list
           '("Atmospheric ponds"
             "Electric gumption socks"
             "Mrs. Leland's embyronic television combustion"
             "Savage gymnatic aggressors"
             "Pharmaceutical pianos"
             "Intravenous retribution champions"))
         (expected-string
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
</html>"))
         (amounts '(10 6 4 9 6 9))
         (*print-pretty* t)
         (generated-string
           (with-html-string
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
                 (:footer ("Last login: ~A" last-login)))))))
    (is (equalp generated-string expected-string))))

(test indent-problem
  (let ((*print-pretty* t))
    (is (equal
         (with-html-string
           (:ul (:li (:a "hai"))))
         (format nil "~
<ul>
 <li><a>hai</a>
</ul>")))

    (is (equal-sans-trailing-spaces
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

;; Local Variables:
;; whitespace-style: nil
;; End:
