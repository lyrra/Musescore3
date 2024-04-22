(use-modules (ice-9 match))


(define (pop-arg-goo args e)
  (define (maybe-pop-arg e)
    (cond
     ((not (assis? 'pop-arg e))
      (set! e (assq-set! e 'pop-arg #t))
      '(raw "pop_arg()"))
     (else
      '(noop))))
  `(begin
     (raw ,(format #f "// pop-arg-goo ~a~%" args))
     (raw "a()")
     ,(maybe-pop-arg e)
     (raw "b()")))

(define (next-arg)
  '(raw "args = s7_cdr(args)"))
