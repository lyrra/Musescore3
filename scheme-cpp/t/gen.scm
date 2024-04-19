(use-modules (ice-9 match))


(define (emit-pop-arg-goo args e)
  (define (maybe-emit-pop-arg e)
    (unless (assis? 'pop-arg e)
      (format %c "    pop_arg();~%")
      (set! e (assq-set! e 'pop-arg #t)))
    e)
  (let ((type (if (pair? args) (car args) #f))
        (name (if (and (pair? args) (pair? (cdr args))) (cadr args) #f))
        (cvar (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args))) (caddr args) "g"))
        (cvarg (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))))
    (format %c "~%    // emit-pop-arg-goo ~a (~a) => ~a~%" name type cvar)
    (format %c "    a();~%")
    (set! e (maybe-emit-pop-arg    e))
    (format %c "    b();~%")
    e))

(define (emit-next-arg args e)
  (format %c "
    // emit-next-arg
    args = s7_cdr(args);
"))

(comp-register-primitive 'pop-arg-goo emit-pop-arg-goo)
(comp-register-primitive 'next-arg emit-next-arg)
