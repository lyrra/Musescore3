;;;; FIX: this belong to /gen

(format #t "---- types-code.scm (running during build time)~%")
(format #t "---- This code generates code that can be~%")
(format #t "---- Loaded during runtime by S7/Guile.~%")

; S7 write does pretty-printing which ellipses output of large lists
(define (write-lst lst p)
  (cond
   ((null? lst))
   ((proper-list? lst)
    (format p "(")
    (format #t "--- ~s ---~%" lst)
    (do ((pair lst (cdr pair)))
        ((null? pair))
      (let ((item (car pair)))
        (if (pair? item)
            (write-lst item p)
            (format p " ~s" item))))
    (format p ")"))
   ((pair? lst)
    (format p "(")
    (write-lst (car lst) p)
    (format p " . ")
    (write-lst (cdr lst) p)
    (format p ")"))
   (else
    (format p " ~s" lst))))

(define (write-types-code p)
  (format p ";;;; This file is generated, by 'mtest/types-code.scm'~%")
  (format p "(define %c-types '")
  (write-lst %c-types p)
  (format p ")~%")
  (format p "(define %c-types-info '")
  (write-lst %c-types-info p)
  (format p ")~%")
  #f)

(define (write-types-code-file output-filename)
  (call-with-output-file output-filename
    write-types-code))
