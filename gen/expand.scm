
(define-syntax e
  (lambda (x)
    (syntax-case x ()
      ((e form)
       #'form))))

(define-syntax define-c-type
  (lambda (x)
    (syntax-case x ()
      ((define-c-type args)
       #'(eval-c-type 'args)))))

