;
; polyfill library for s7
; for running guile scheme on s7
;

(define-macro (match expr case1)
  (let ((lst (car case1))
        (body (cdr case1)))
  `(let ((ex ,expr))
     (define (flatten lst)
       (cond ((null? lst) ())
             ((pair? lst)
              (if (pair? (car lst))
                  (append (flatten (car lst)) (flatten (cdr lst)))
                  (cons (car lst) (flatten (cdr lst)))))
             (#t lst)))

     (define (structures-equal? l1 l2)
       (if (pair? l1)
           (and (pair? l2)
                (structures-equal? (car l1) (car l2))
                (structures-equal? (cdr l1) (cdr l2)))
           (not (pair? l2))))

     ; 20230728, disabled, fails on good match (a b c) <= (1 2 (3))
     ;(if (not (structures-equal? ',lst ex))
     ;    (error 'test-error "~A and ~A do not match" ',lst ex))

     (let ((names (flatten ',lst))
           (vals (flatten ex)))
       (apply (eval (list 'lambda names '(begin ,@body))) vals)))))

(define (assq-ref lst key)
  (if (assoc key lst)
      (cdr (assoc key lst))
      #f))


(define (filter proc lst)
  (map (lambda (x) (if (proc x) x (values))) lst))

(define (assq-set! lst key val)
  (let ((pair (assq key lst)))
    (cond
     (pair
      (set-cdr! pair val)
      lst)
     (else
      (cons (cons key val) lst)))))

(define (p-open-output-file file mode)
  (open-output-file file mode))
