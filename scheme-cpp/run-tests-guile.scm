(define %verbose #f)

(load "extras.scm")

(begin
  (do ((pair (command-line) (cdr pair)))
      ((eq? '() pair))
    (let ((arg (car pair)))
      (cond
       ((string=? "-v" arg)
        (set! %verbose #t))))))

(define %h #f)
(define %c #f)

(load "comp.scm")

(load "t/gen.scm")
(load "t/tests.scm")
