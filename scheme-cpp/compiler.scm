;;;;
;;;; scheme-pseudo-code to c compiler
;;;;

(define (c-ify-name sname)
  (let ((str "")
        (sname (if (string? sname) sname (symbol->string sname))))
    (do ((i 0 (+ 1 i)))
        ((>= i (string-length sname)))
      (set! str (format #f "~a~a" str
                        (let ((s (substring sname i (+ i 1))))
                          (if (string=? s "-") "_" s)))))
    str))


; auxiliary stuff
(define %comp-export '())

(define (comp-begin)
  (set! %comp-export '()))

(define %primitives '())

(define (register-primitive name fun)
  (set! %primitives
        (cons (cons name fun)
              %primitives)))

;;;
;;; intermediate-representation
;;;

(define (emit-c-stend)
  (format %c ";~%"))

(define (emit-c-enum ir)
  (let ((type (car ir))
        (csubtype (cadr ir))
        (types (caddr ir)))
    (format %h "enum class GOO_TYPE : uint64_t {~%")
    (format %h "  NIL = 0,~%")
    (for-each (lambda (name)
                (format %h "  ~a,~%" name))
              types)
    (format %h "  END~%};~%")))

(define (emit-c-begin ir args)
  (let ((n 0)
        (num-stmt (length ir)))
    (for-each (lambda (line)
                (format %c "    ")
                (set! n (+ 1 n))
                (emit-c line
                        (if (and (= num-stmt n) (memq 'tcp args))
                            args
                            '()))
                (emit-c-stend))
              ir)))

(define (emit-c-let ir args)
  (let ((n 1)
        (num-stmt (length ir)))
    (format %c "  {~%")
    (for-each (lambda (line)
                (let ((name (car line))
                      (type (if (pair? (cdr line)) (cadr line) #f))
                      (init (if (and (pair? (cdr line)) (pair? (cddr line))) (caddr line) #f)))
                  (format %c "    ~a ~a" (or type "auto") name)
                  (cond
                   (init
                    (format %c " = ")
                    (emit-c init)
                    (format %c ";~%"))
                   (else
                    (format %c ";~%")))))
              (car ir))
    (for-each (lambda (line)
                (format %c "    ")
                (set! n (+ 1 n))
                (emit-c line
                        (if (and (= num-stmt n) (memq 'tcp args))
                            args
                            '()))
                (emit-c-stend))
              (cdr ir))
    (format %c "  }~%")))

(define (emit-c-set! ir args)
  (let ((dst (car ir))
        (src (cadr ir)))
    (format %c "  ~a = " dst)
    (emit-c src args)))

(define (emit-c-if ir args)
  (let ((c (car ir))
        (t (cadr ir))
        (e? (cddr ir)))
    (format %c "  if (")
    (emit-c c)
    (format %c") {~%")
    (if (memq 'tcp args)
        (emit-c (list 'return t args))
        (emit-c t))
    (emit-c-stend)
    (when (not (null? e?))
      (format %c "  } else {~%")
      (if (memq 'tcp args)
          (emit-c (list 'return (car e?) args))
          (emit-c (car e?)))
      (emit-c-stend))
    (format %c "  }~%"))
  #f)

(define (emit-c-when ir args)
  (let ((c (car ir))
        (t (cdr ir)))
    (format %c "if (")
    (emit-c c)
    (format %c") {~%")
    (let ((n 0)
          (num-stmt (length t)))
      (for-each (lambda (line)
                  (format %c "    ")
                  (set! n (+ 1 n))
                  (if (and (= n num-stmt) (memq 'tcp args))
                      (emit-c (list 'return line) args)
                      (emit-c line))
                  (emit-c-stend))
                t))
    (format %c "}~%")))

(define (emit-c-cond ir args)
  (let ((n 0))
    (for-each (lambda (clause)
                (cond
                 ((= n 0) (format %c "  if ("))
                 ((eq? 'else (car clause))
                  (format %c "  } else {~%"))
                 (else
                  (format %c "  } else if (")))
                (set! n (+ 1 n))
                (when (not (eq? 'else (car clause)))
                  (emit-c (car clause))
                  (format %c ") {~%"))
                (let ((n 0)
                      (num-stmt (length (cdr clause))))
                  (for-each (lambda (line)
                              (format %c "    ")
                              (set! n (+ 1 n))
                              (if (and (= n num-stmt) (memq 'tcp args))
                                  (emit-c (list 'return line) args)
                                  (emit-c line))
                              (emit-c-stend))
                            (cdr clause))))
              ir)
    (format %c "  }~%")))

(define (emit-c-switch ir args)
  (let ((cnd (car ir))
        (cas (cdr ir)))
    (format %c "switch ~a {~%" cnd)
    (for-each (lambda (expr)
                (cond
                 ((eq? 'default (car expr))
                  (format %c "  default:~%  ")
                  (if (memq 'tcp args)
                      (emit-c (list 'return
                                    (cadr expr) args))
                      (emit-c (cadr expr) args))
                  (emit-c-stend))
                 (else
                  (format %c "  case ~a:~%" (cadr expr))
                  (for-each (lambda (ir)
                              (if (memq 'tcp args)
                                  (emit-c (list 'return
                                                ir args))
                                  (emit-c ir args))
                              (emit-c-stend))
                            (cddr expr)))))
              cas)
    (format %c "}~%")))

(define (emit-c-body ir)
  (let ((n 0)
        (num-stmt (length ir)))
    (for-each (lambda (line)
                (set! n (+ 1 n))
                (if (emit-c line (if (= n num-stmt) '(tcp) '()))
                    (emit-c-stend)))
              ir)))

(define (emit-c-fun ir)
  (let* ((scname (car ir))
         (args (cadr ir))
         (rett (caddr ir))
         (body (cadddr ir)))
    (format %h "~a ~a(" rett (cadr scname))
    (format %h "~a" (string-interleave args ", "))
    (format %h ");~%")
    (format %c "~%~a ~a(" rett (cadr scname))
    (format %c "~a" (string-interleave args ", "))
    (format %c ")~%{~%")
    (emit-c-body body)
    (format %c "}~%~%")))

(define (emit-infix op ir args)
  (let ((a (car ir))
        (b (cadr ir)))
    (emit-c a)
    (format %c " ~a " op)
    (emit-c b)))

(define (emit-raw ir args)
  (format %c "  ~a" (car ir))
  '())

(define (emit-c-funcall ir args)
  (if (memq 'tcp args)
      (format %c "  return "))
  (format %c "~a(" (car ir))
  (let ((n 0))
    (for-each (lambda (arg)
                (if (> n 0) (format %c ", "))
                (emit-c arg)
                (set! n (+ n 1)))
              (cdr ir)))
  (format %c ")")
  '())

(define (emit-c-return ir)
  (format %c "  return ")
  (emit-c (car ir))
  '())

(define (emit-c ir . args)
  (if (not (null? args)) (set! args (car args)))
  (cond
   ((pair? ir)
    (case (car ir)
      ((noop) #f)
      ((defcenum)
       (emit-c-enum (cdr ir)))
      ((defcfun)
       (emit-c-fun (cdr ir)))
      ((return)
       (emit-c-return (cdr ir)))
      ((begin)
       (emit-c-begin (cdr ir) args))
      ((let)
       (emit-c-let (cdr ir) args))
      ((set!)
       (emit-c-set! (cdr ir) args))
      ((if)
       (emit-c-if (cdr ir) args))
      ((when)
       (emit-c-when (cdr ir) args))
      ((cond)
       (emit-c-cond (cdr ir) args))
      ((switch)
       (emit-c-switch (cdr ir) args))
      ; reserved functions
      ((==) (emit-infix '== (cdr ir) args))
      ((&&) (emit-infix '&& (cdr ir) args))
      ((>=) (emit-infix '>= (cdr ir) args))
      ((raw)
       (emit-raw (cdr ir) args))
      (else
       (emit-c-funcall ir args))))
   (else
    (if (memq 'tcp args)
        (format %c "  return "))
    (format %c "~a" ir)
    '())))

;;;
;;; compile pseudo to intermediate-representation
;;;

(define (ir-comp-cenum type csubtype types)
  (list 'defcenum type csubtype types))

(define (comp-cenum expr)
  (let ((type (car expr))
        (csubtype (cadr expr))
        (types (caddr expr)))
  (ir-comp-cenum type csubtype types)))

(define (ir-comp-cfun names args rett body)
  (let* ((sname (car names))
         (cname (c-ify-name sname)))
    `(defcfun (,sname ,cname) ,args ,rett
       ,(map (lambda (line)
               line)
             body))))

(define (comp-cfun expr)
  (let ((names (car expr))
        (args (cadr expr))
        (rett (caddr expr))
        (body (cdddr expr)))
    (let ((ir (ir-comp-cfun names args rett body)))
      ir)))

;;;
;;; evaluate pseudo code
;;;

(define (evalc-comp expr)
  (comp-begin)
  (cond
   ((pair? expr)
    (case (car expr)
      ((defcenum)
       (comp-cenum (cdr expr)))
      ((defcfun)
       (comp-cfun (cdr expr)))))))

(define (evalc expr)
  ; compile expression into immediate-representation
  (let ((ir (evalc-comp expr)))
    ; convert and write IR into C
    (emit-c ir)))
