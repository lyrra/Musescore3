;;;;
;;;; scheme-pseudo-code to c compiler
;;;;

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

(define (emit-c-fun ir)
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

(define (emit-c ir)
  (cond 
   ((pair? ir)
    (case (car ir)
      ((defcenum)
       (emit-c-enum (cdr ir)))
      ((defcfun)
       (emit-c-fun (cdr ir)))
      ((return)
       (format %c "  return ")
       (emit-c (cadr ir))
       (format %c ";~%"))
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

(define (ir-comp-cfun names args body)
  (let* ((sname (car names))
         (cname (c-ify-name sname))
         (lines (length body)))
    (list 'defcfun (list sname cname) args
          (let ((n 0))
            (map (lambda (line)
                   (set! n (+ n 1))
                   (if (= n lines) ; last line
                       (append (list 'return line))
                       line))
                 body)))))

(define (comp-cfun expr)
  (let ((names (car expr))
        (args (cadr expr))
        (body (cddr expr)))
  (ir-comp-cfun names args body)))

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
       (comp-cfun (cdr expr)))))))

(define (evalc expr)
  (let ((ir (evalc-comp expr)))
    (emit-c ir)))
