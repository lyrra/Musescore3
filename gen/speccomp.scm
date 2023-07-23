
(define (write-dat dat proc)
  (cond
   ((null? dat))
   ((number? dat)
    (format proc "~a" dat))
   ((symbol? dat)
    (format proc "~a" dat))
   ((string? dat)
    (format proc "~a" dat))
   ((pair? dat)
    (write-dat (car dat) proc)
    (write-dat (cdr dat) proc))))

(define (compile-spec form proc)
  (let ((dat (primitive-eval form)))
    (write-dat dat proc)))

(define (compile-specification inproc outproc)
  (do ((f (read inproc) (read inproc)))
      ((eof-object? f))
    (compile-spec f outproc)))

