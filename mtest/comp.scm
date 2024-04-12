;;;;
;;;; scheme-pseudo-code to c compiler
;;;;

(define (%string-join sep lst cnt acc)
  (if (null? lst)
      acc
      (%string-join sep (cdr lst) (+ 1 cnt) (format #f "~a~a~a"
                                                    acc
                                                    (if (> cnt 0) sep "")
                                                    (car lst)))))

(define (string-join sep lst)
  (%string-join ", " lst 0 ""))

;;;
;;; intermediate-representation
;;;

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

(define (emit-c-if ir)
  (let ((c (car ir))
        (t (cadr ir))
        (e? (cddr ir)))
    (format %c "if (")
    (emit-c c)
    (format %c") {~%")
    (emit-c t)
    (cond
     ((null? e?)
      (format %c "}~%"))
     (else
      (format %c "} else {~%")
      (emit-c (car e?))
      (format %c "}~%")))))

(define (emit-c-switch ir)
  (let ((cnd (car ir))
        (cas (cdr ir)))
    (format %c "switch ~a {~%" cnd)
    (for-each (lambda (expr)
                (cond
                 ((eq? 'default (car expr))
                  (format %c "  default:~%  ")
                  (emit-c (cadr expr))
                  (format %c "~%"))
                 (else
                  (format %c "  case ~a:~%" (cadr expr))
                  (for-each emit-c (cddr expr)))))
              cas)
    (format %c "}~%")))

(define (emit-c-reg ir)
  (let* ((scname (car ir))
         (args (cadr ir))
         (body (caddr ir)))
    (set! %export-to-scheme2
          (cons (list (car scname) (cadr scname) 0 0)
                %export-to-scheme2))
    (format %h "s7_pointer ~a (s7_scheme *sc, s7_pointer args);~%" (cadr scname))
    (format %c "s7_pointer ~a (s7_scheme *sc, s7_pointer args)~%{~%" (cadr scname))
    (for-each (lambda (line)
                (emit-c line))
              body)
    (format %c "}~%~%")))

(define (emit-c-fun ir)
  (let* ((scname (car ir))
         (args (cadr ir))
         (rett (caddr ir))
         (body (cadddr ir)))
    (format %h "~a ~a(" rett (cadr scname))
    (format %h "~a" (string-join ", " args))
    (format %h ");~%")
    (format %c "~%~a ~a(" rett (cadr scname))
    (format %c "~a" (string-join ", " args))
    (format %c ")~%{~%")
    (for-each (lambda (line)
                (emit-c line))
              body)
    (format %c "}~%~%")))

(define (emit-== ir)
  (let ((a (car ir))
        (b (cadr ir)))
    (emit-c a)
    (format %c " == ")
    (emit-c b)))

(define (emit-c ir)
  (cond 
   ((pair? ir)
    (case (car ir)
      ((defcenum)
       (emit-c-enum (cdr ir)))
      ((defcfun)
       (emit-c-fun (cdr ir)))
      ((defcreg)
       (emit-c-reg (cdr ir)))
      ((return)
       (format %c "  return ")
       (emit-c (cadr ir))
       (format %c ";~%"))
      ((if)
       (emit-c-if (cdr ir)))
      ((switch)
       (emit-c-switch (cdr ir)))
      ; reserved functions
      ((==)
       (emit-== (cdr ir)))
      (else ; c-call
       (format %c "~a(" (car ir))
       (let ((n 0))
         (for-each (lambda (arg)
                     (if (> n 0) (format %c ", "))
                     (emit-c arg)
                     (set! n (+ n 1)))
                   (cdr ir)))
       (format %c ")"))))
   (else
    (format %c "~a" ir))))

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

(define (ir-maybe-return expr)
  (if (pair? expr)
      (let ((f (car expr)))
        (cond
         ((memq f '(if switch)) #f)
         (else #t)))
      #t))

(define (ir-comp-cfun names args rett body)
  (let* ((sname (car names))
         (cname (c-ify-name sname))
         (lines (length body)))
    (list 'defcfun (list sname cname) args rett
          (let ((n 0))
            (map (lambda (line)
                   (set! n (+ n 1))
                   (if (and (= n lines) ; last line
                            (ir-maybe-return line))
                       (append (list 'return line))
                       line))
                 body)))))

(define (ir-comp-creg names args body)
  (let* ((sname (car names))
         (cname (c-ify-name sname))
         (lines (length body)))
    (list 'defcreg (list sname cname) args
          (let ((n 0))
            (map (lambda (line)
                   (set! n (+ n 1))
                   (if (and (= n lines) ; last line
                            (ir-maybe-return line))
                       (append (list 'return line))
                       line))
                 body)))))

(define (comp-cfun expr)
  (let ((names (car expr))
        (args (cadr expr))
        (rett (caddr expr))
        (body (cdddr expr)))
    (let ((ir (ir-comp-cfun names args rett body)))
      ir)))

(define (comp-creg expr)
  (let ((names (car expr))
        (args (cadr expr))
        (body (cddr expr)))
    (let ((ir (ir-comp-creg names args body)))
      ir)))

;;;
;;; evaluate pseudo code
;;;

(define (evalc-comp expr)
  (cond
   ((pair? expr)
    (case (car expr)
      ((defcenum)
       (comp-cenum (cdr expr)))
      ((defcfun)
       (comp-cfun (cdr expr)))
      ((defcreg)
       (comp-creg (cdr expr)))))))

(define (evalc expr)
  ; compile expression into immediate-representation
  (let ((ir (evalc-comp expr)))
    ; convert and write IR into C
    (emit-c ir)))
