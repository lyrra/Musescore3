(define-syntax match1
  (syntax-rules ()
    ((_ form (pattern . body))
     (apply (lambda pattern . body) form))))

(define %export-to-scheme-getset '()) ; get/setters
(define %export-to-scheme-getset2 '()) ; get/setters
(define %export-to-scheme-get '())    ; getters only
(define %export-to-scheme2 '())

(define %objects '())

(define %compile-env '())

(define-syntax defcompile
  (syntax-rules ()
    ((_ . body)
     (begin
       (set! %compile-env '())
       (evalc . body)))))

(define-syntax defcreg
  (syntax-rules ()
    ((_ (sname) (num-req-args num-opt-args) . body)
     (compile-register-cfun sname num-req-args num-opt-args
                            (list . body)))
    ((_ (sname) (num-req-args) . body)
     (compile-register-cfun sname num-req-args 0
                            (list . body)))
    ((_ (sname) () . body)
     (compile-register-cfun sname 0 0
                            (list . body)))
    ((_ sname (req-args ...) (opt-args ...) body ...)
     (compreg-cfun sname
                   '(req-args ...)
                   '(opt-args ...)
                   (list body ...)))))

(define-syntax define-object
  (syntax-rules ()
    ((k objname . args)
     (register-object 'objname 'args))))

(define (register-object obj-name args);  c-type make-args methods)
  (let ((c-type  (cadr (assq 'c-type args)))
        (methods  (cdr (assq 'methods args)))
        (make-args (assq-ref args 'make-args)))
    (for-each (lambda (meth)
                (format #t "    method: ~s~%" meth)
                )
              methods)
    (format #t "registering object ~a~%" obj-name)
    (set! %objects
          (assq-set! %objects obj-name
                     `((c-type . ,c-type)
                       (make-args . ,make-args)
                       (methods . ,methods))))))

(define-syntax emit-raw
  (syntax-rules ()
    ((_ . args)
     (list 'raw (format #f . args)))))

(define (compile-register-cfun sname num-req-args num-opt-args body)
  (let ((cname (c-ify-name sname)))
    (set! %export-to-scheme2
          (cons (list sname cname num-req-args num-opt-args)
                %export-to-scheme2))
    (append `(defcfun (,sname ,cname)
               ("s7_scheme *sc" "s7_pointer args")
               "s7_pointer")
            body)))

(define (compile-args at args)
  (map (lambda (arg)
         (let* ((n (length arg))
                (a1 (if (> n 0) (car arg) #f))
                (a2 (if (> n 1) (cadr arg) #f))
                (a3 (if (> n 2) (caddr arg) #f)))
           (cond
            ((and (= n 2) (eq? 'goo (cadr arg)))
             (cond
              ((or (eq? 's at) (eq? 'r at))
               `((type . goo) (ctype . ,(car arg))))
              (else
               (error "bad goo-2 arg" at arg))))
            ((and (= n 3) (eq? 'goo (cadr arg)))
             (cond
              ((eq? 's at)
               `((type . goo) (ctype . ,(car arg))))
              ((eq? 'i at)
               `((type . goo) (ctype . ,(caddr arg)) (name . ,(car arg))))
              ((eq? 'r at)
               `((type . goo) (ctype . ,(car arg)) (elm . ,(caddr arg))))
              (else
               (error "bad goo-3 arg" at arg))))
            ; implementation types
            ((and (eq? 'i at) (= n 2) (eq? 'sym (cadr arg)))
             `((type . sym) (name . ,(car arg))))
            ((and (eq? 'i at) (= n 2) (eq? 'int (cadr arg)))
             `((type . int) (name . ,(car arg))))
            ((and (eq? 'i at) (= n 2) (eq? 'bool (cadr arg)))
             `((type . bool) (name . ,(car arg))))
            ; signature types
            ((and (eq? 's at) (= n 1) (eq? 'bool (car arg)))
             `((type . bool)))
            ((and (eq? 's at) (= n 1) (eq? 'int (car arg)))
             `((type . int)))
            ((and (eq? 's at) (= n 2) (eq? 'int (car arg)) (eq? 'track (cadr arg)))
             `((type . int) (track . #t)))
            ((and (eq? 's at) (= n 2) (eq? 'int (car arg)) (eq? 'opt (cadr arg)))
             `((type . int) (opt . #t)))
            ((and (eq? 's at) (= n 2) (eq? 'int (car arg)) (symbol? (cadr arg)))
             `((type . int) (ctype . ,(cadr arg))))
            ((and (eq? 's at) (= n 3) (eq? 'int (car arg)) (eq? 'opt (caddr arg)))
             `((type . int) (ctype . ,(cadr arg)) (opt . #t)))
            ((and (eq? 's at) (= n 1) (eq? 'real (car arg)))
             `((type . real) (ctype . ,(car arg))))
            ((and (eq? 's at) (= n 2) (eq? 'sym (car arg)) (eq? 'opt (cadr arg)))
             `((type . sym) (opt . #t)))
            ((and (eq? 's at) (= n 2) (eq? 'sym (car arg)))
             `((type . sym) (ctype . ,(cadr arg))))
            ((and (eq? 's at) (= n 3) (eq? 'sym (car arg)) (eq? 'opt (caddr arg)))
             `((type . sym) (ctype . ,(cadr arg)) (opt . #t)))
            ((and (eq? 's at) (= n 1) (string? (car arg)))
             `((type . goo) (ctype . ,(car arg))))
            ((and (eq? 's at) (= n 2) (string? (car arg)) (eq? 'deref (cadr arg)))
             `((type . goo) (ctype . ,(car arg)) (deref . #t)))
            ; return types
            ((and (eq? 'r at) (= n 1) (eq? 'int (car arg)))
             `((type . int)))
            ((and (eq? 'r at) (= n 1) (eq? 'bool (car arg)))
             `((type . bool)))
            ((and (eq? 'r at) (= n 2) (eq? 'sym (car arg)))
             `((type . sym) (ctype . ,(cadr arg))))
            ((and (eq? 'r at) (= n 1) (eq? 'QString (car arg)))
             `((type . QString)))
            ((and (eq? 'r at) (= n 1) (string? (car arg)))
             `((type . goo) (ctype . ,(car arg))))
            ((and (eq? 'r at) (= n 2) (string? (car arg)))
             (cond
              ((eq? 'stack (cadr arg))
               `((type . goo) (ctype . ,(car arg)) (stack . #t)))
              ((eq? 'goo (cadr arg))
               `((type . goo) (ctype . ,(car arg))))
              (else
               (error "bad goo arg" at arg))))
            (else (error "bad arg" at arg)))))
       args))

(define (compreg-cfun sname req-args opt-args body)
  (let ((req-args (compile-args 'i req-args))
        (opt-args (compile-args 'i opt-args)))
    (set! %compile-env '())
    (let ((cname (c-ify-name sname)))
      (set! %export-to-scheme2
            (cons (list sname cname (length req-args) (length opt-args))
                  %export-to-scheme2))
      (append `(defcfun (,sname ,cname)
                 ("s7_scheme *sc" "s7_pointer args")
                 "s7_pointer")
              (list (emit-code-get-argso #t 's #f 1 req-args opt-args body))))))

(define (emit-code-get-argso an? at na? n req-args opt-args body)
      (append
       `(begin (raw ,(format #f "// ---- emit-code-get-argso ~a ----" n)))
       (cond
        ((and (null? req-args) (null? opt-args))
         body)
        ((not (null? req-args))
         (append
          (let* ((arg (car req-args))
                 (varname (if an? (assq-ref arg 'name) (format #f "x~a" n))))
            (set! %compile-env (assq-set! %compile-env 'argsPairKnow #f))
            (list (list 'begin
                        (if (> n 1) '(set! args (s7_cdr args)) '(begin)))
                  (case (assq-ref arg 'type)
                    ((bool)
                     (let ((s (assis? 's %compile-env)))
                       (set! %compile-env (assq-set! %compile-env 's #t))
                       `(begin
                          (if (!(s7_is_pair args)) (return (s7_f sc)))
                          (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                          (if (! (s7_is_boolean s)) (return (s7_f sc)))
                          (raw ,(format #f "bool ~a = s7_boolean(sc, s);~%" varname))
                          ,(emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body))))
                    ((int)
                     (let ((s (assis? 's %compile-env)))
                       (set! %compile-env (assq-set! %compile-env 's #t))
                       `(begin
                          ,(maybe-emit-args-check)
                          (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                          (if (! (s7_is_integer s)) (return (s7_f sc)))
                          (raw ,(format #f "int ~a = s7_integer(s);~%" varname))
                          ,(emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body))))
                    ((real)
                     (let ((s (assis? 's %compile-env)))
                       (set! %compile-env (assq-set! %compile-env 's #t))
                       `(begin
                          ,(maybe-emit-args-check)
                          (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                          (if (! (s7_is_real s)) (return (s7_f sc)))
                          (raw ,(format #f "double ~a = s7_real(s);~%" varname))
                          ,(emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body))))
                    ((sym)
                     (let* ((conv (assq-ref arg 'ctype))
                            (stype (if conv (typeinfo-meta-get conv 'c-type-cons) #f))
                            (s (assis? 's %compile-env)))
                       (set! %compile-env (assq-set! %compile-env 's #t))
                       (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
                       (if stype
                         `(begin
                            (if (!(s7_is_pair args)) (return (s7_f sc)))
                            (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                            (if (! (s7_is_symbol s)) (return (s7_f sc)))
                            (raw ,(format #f "const char* t~a = s7_symbol_name(s);~%" n))
                            (raw ,(format #f "Ms::~a ~a = string_to_~a(t~a)" stype varname conv n))
                            ,(emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body))
                         `(begin
                            ,(maybe-emit-args-check)
                            (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                            (if (! (,(string->symbol (format #f "s7_is_symbol")) s)) (return (s7_f sc)))
                            (raw ,(format #f "const char* ~a = s7_symbol_name(s);~%" varname))
                            ,(emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body)))))
                    (else
                     (emit-pop-arg-goo2 (= n 1) #f #f #f
                      (list (assq-ref arg 'ctype) varname)
                      (format #f "g~a" n)
                      (emit-code-get-argso an? at na? (+ 1 n) (cdr req-args) opt-args body))))))))
        ((and (null? req-args) (not na?))
         (append
           (list (list 'raw (format #f "// emit numArgs and moreargs")))
           (list (if (> (length opt-args) 0)
                     (list 'raw (format #f "bool moreArgs = true"))
                     '(noop)))
           (list (list 'raw (format #f "int numArgs = ~a" n)))
           (list (emit-code-get-argso an? at #t n req-args opt-args body))))
        ((null? req-args)
         (let* ((arg (car opt-args))
                (varname (if an? (assq-ref arg 'name) (format #f "x~a" n))))
           (list
           (list 'begin
                 (emit-maybe-next-arg)
                 (case (assq-ref arg 'type)
                   ((int)
                    (let ((s (assis? 's %compile-env)))
                      (set! %compile-env (assq-set! %compile-env 's #t))
                      `(begin
                         (raw "// ---- emit int arg")
                         ,(maybe-emit-args-check)
                         (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                         (if (! (s7_is_integer s)) (return (s7_f sc)))
                         (raw ,(format #f "int ~a = s7_integer(s);~%" varname))
                         ,(emit-code-get-argso an? at na? (+ 1 n) req-args (cdr opt-args) body))))
                   ((sym)
                    (format #t "arg: ~s~%" arg)
                    (let ((conv (assq-ref arg 'ctype))
                          (stype (typeinfo-meta-get (assq-ref arg 'ctype) 'c-type-cons))
                          (cargname (format #f "t~a" n))
                          (s (assis? 's %compile-env)))
                      (set! %compile-env (assq-set! %compile-env 's #t))
                      (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
                      `(begin
                         (raw "// ---- emit sym arg")
                         (if (!(s7_is_pair args)) (return (s7_f sc)))
                         (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                         (if (! (s7_is_symbol s)) (return (s7_f sc)))
                         (raw ,(format #f "const char* ~a = s7_symbol_name(s);~%" cargname))
                         (raw ,(format #f "Ms::~a ~a = string_to_~a(t~a)" stype varname conv n))
                         ,(emit-code-get-argso an? at na? (+ 1 n) req-args (cdr opt-args) body))))
                   (else
                    (emit-pop-arg-goo2 (= n 1) #t #t #t
                                       (raw "// ---- emit goo arg")
                                       (list (assq-ref arg 'ctype) varname)
                                       (format #f "g~a" n)
                     (raw "// ---- emit sym arg")
                     (emit-code-get-argso an? at na? (+ 1 n) req-args (cdr opt-args) body)))))))))))

(define (emit-code-get-args2 at n cargs body)
      (append
       '(begin (raw "// ---- emit-code-get-args ----"))
       (if (null? cargs)
           body
           (append
            (let* ((arg (car cargs))
                   (argtype (assq-ref arg 'type)))
              (set! %compile-env (assq-set! %compile-env 'argsPairKnow #f))
              (list (list 'begin
               '(set! args (s7_cdr args)))
               (case argtype
                 ((bool)
                  ; emit-pop-arg-bool
                  (let* ((cargname (format #f "x~a" n))
                         (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       (if (!(s7_is_pair args)) (return (s7_f sc)))
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_boolean s)) (return (s7_f sc)))
                       (raw ,(format #f "bool ~a = s7_boolean(sc, s);~%" cargname))
                       ,(emit-code-get-args2 at (+ 1 n) (cdr cargs) body))))
                 ((int)
                  (let ((cargname (format #f "x~a" n))
                        (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       ,(maybe-emit-args-check)
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_integer s)) (return (s7_f sc)))
                       (raw ,(format #f "int ~a = s7_integer(s);~%" cargname))
                       ,(emit-code-get-args2 at (+ 1 n) (cdr cargs) body))))
                 ((real)
                  (let ((cargname (format #f "x~a" n))
                        (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       ,(maybe-emit-args-check)
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_real s)) (return (s7_f sc)))
                       (raw ,(format #f "double ~a = s7_real(s);~%" cargname))
                       ,(emit-code-get-args2 at (+ 1 n) (cdr cargs) body))))
                 ((sym)
                  (let* ((conv (assq-ref arg 'ctype))
                         (stype (typeinfo-meta-get conv 'c-type-cons))
                         (cargname (format #f "t~a" n))
                         (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
                    `(begin
                       (if (!(s7_is_pair args)) (return (s7_f sc)))
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_symbol s)) (return (s7_f sc)))
                       (raw ,(format #f "const char* ~a = s7_symbol_name(s);~%" cargname))
                       (raw ,(format #f "Ms::~a x~a = string_to_~a(t~a)" stype n conv n))
                       ,(emit-code-get-args2 at (+ 1 n) (cdr cargs) body))))
                 (else
                  (emit-pop-arg-goo2 (= n 1) #f #f #f
                   (list (assq-ref arg 'ctype) (format #f "x~a" n))
                   (format #f "g~a" n)
                   (emit-code-get-args2 at (+ 1 n) (cdr cargs) body))))))))))

(define (emit-code-get-args at n cargs body)
      (append
       '(begin (raw "// ---- emit-code-get-args ----"))
       (if (null? cargs)
           body
           (append
            (let* ((arg (car cargs))
                   (argtype (car arg)))
              (set! %compile-env (assq-set! %compile-env 'argsPairKnow #f))
              (list (list 'begin
               '(set! args (s7_cdr args)))
               (case argtype
                 ((bool)
                  ; emit-pop-arg-bool
                  (let* ((cargname (format #f "x~a" n))
                         (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       (if (!(s7_is_pair args)) (return (s7_f sc)))
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_boolean s)) (return (s7_f sc)))
                       (raw ,(format #f "bool ~a = s7_boolean(sc, s);~%" cargname))
                       ,(emit-code-get-args at (+ 1 n) (cdr cargs) body))))
                 ((int)
                  (let ((cargname (format #f "x~a" n))
                        (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       ,(maybe-emit-args-check)
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_integer s)) (return (s7_f sc)))
                       (raw ,(format #f "int ~a = s7_integer(s);~%" cargname))
                       ,(emit-code-get-args at (+ 1 n) (cdr cargs) body))))
                 ((real)
                  (let ((cargname (format #f "x~a" n))
                        (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    `(begin
                       ,(maybe-emit-args-check)
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_real s)) (return (s7_f sc)))
                       (raw ,(format #f "double ~a = s7_real(s);~%" cargname))
                       ,(emit-code-get-args at (+ 1 n) (cdr cargs) body))))
                 ((sym)
                  (format #t ".sym-arg: ~s (~a)~%" arg at)
                  (let* ((conv (if (eq? 's at) (cadr arg) (cadr arg))) ; fix
                         (stype (typeinfo-meta-get conv 'c-type-cons))
                         (cargname (format #f "t~a" n))
                         (s (assis? 's %compile-env)))
                    (set! %compile-env (assq-set! %compile-env 's #t))
                    (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
                    `(begin
                       (if (!(s7_is_pair args)) (return (s7_f sc)))
                       (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
                       (if (! (s7_is_symbol s)) (return (s7_f sc)))
                       (raw ,(format #f "const char* ~a = s7_symbol_name(s);~%" cargname))
                       (raw ,(format #f "Ms::~a x~a = string_to_~a(t~a)" stype n conv n))
                       ,(emit-code-get-args at (+ 1 n) (cdr cargs) body))))
                 (else
                  (emit-pop-arg-goo2 (= n 1) #f #f #f
                   (list (car arg) (format #f "x~a" n))
                   (format #f "g~a" n)
                   (emit-code-get-args at (+ 1 n) (cdr cargs) body))))))))))

(define (emit-registered-object pair)
  (let* ((name (car pair))
         (args (cdr pair))
         (c-type (assq-ref args 'c-type))
         (make-args (assq-ref args 'make-args))
         (methods (assq-ref args 'methods)))
    (define (make-callargs2 args)
        (let ((n 0) (str ""))
          (for-each (lambda (arg)
                      (set! n (+ 1 n))
                      (set! str (format #f "~a~a~ax~a"
                                        str (if (= n 1) "" ", ")
                                        (if (assq-ref arg 'deref) "*" "") ; c-ref(&), need to ref pointer
                                        n)))
                   args)
          str))
    (define (make-callargs args)
        (let ((n 0) (str ""))
          (for-each (lambda (arg)
                      (set! n (+ 1 n))
                      (set! str (format #f "~a~a~ax~a"
                                        str (if (= n 1) "" ", ")
                                        (if (memq 'deref arg) "*" "") ; c-ref(&), need to ref pointer
                                        n)))
                   args)
          str))
    (define (form-callargs2 args)
      (let ((n 0))
        (map (lambda (arg)
               (set! n (+ 1 n))
               (format #f "~ax~a"
                       ; c-ref(&), need to ref pointer
                       (if (assq-ref arg 'deref) "*" "")
                       n))
             args)))
    (define (form-callargs args)
      (let ((n 0))
        (map (lambda (arg)
               (set! n (+ 1 n))
               (format #f "~ax~a"
                       ; c-ref(&), need to ref pointer
                       (if (memq 'deref arg) "*" "")
                       n))
             args)))
    (define (form-callargs-max args numargs)
      (let ((n 0) (lst '()))
        (for-each (lambda (arg)
                    (set! n (+ 1 n))
                    (if (<= n numargs)
                        (set! lst
                              (cons (format #f "~ax~a"
                                            ; c-ref(&), need to ref pointer
                                            (if (memq 'deref arg) "*" "")
                                            n)
                                    lst))))
             args)
        (reverse lst)))
    (define (make-optional-args args)
      (filter (lambda (arg)
                (assq-ref arg 'opt))
              args))
    (define (make-required-args args)
      (filter (lambda (arg)
                (not (assq-ref arg 'opt)))
              args))
    (define (get-string lst)
      (let ((ss (filter string? lst)))
        (if (null? ss)
          #f
          (car ss))))
    (define (get-symbol lst sym)
      (let ((ss (filter (lambda (elm) (eq? sym elm)) lst)))
        (if (null? ss)
          #f
          (car ss))))
    (define (emit-code-funcall4 setter? info cname req-args opt-args rets)
      (let* ((cargs (append req-args opt-args))
             (methtype (cond ((null? rets) #f)
                             ;((assq-ref? eq? (car rets)) (car rets))
                             ;((assq-ref rets 'ctype)
                             ((and (eq? 'sym (assq-ref rets 'type)) (assq-ref rets 'ctype))
                              (typeinfo-meta-get (assq-ref rets 'ctype) 'c-type-cons))
                             (else
                              (or (assq-ref rets 'ctype)
                                  (assq-ref rets 'type)))))
             (cmethtype (case methtype
                          ((real) 'double)
                          (else methtype)))
             (cargsstr (make-callargs2 cargs))
             (cargslst (form-callargs2
                        (if (and setter? (memq 'ref info))
                            (cons (cons (cons 'deref #t) (car cargs)) (cdr cargs))
                            cargs)))
             (methexpr (cond
                        ((get-string info) (get-string info))
                        ; this avoids constructing an ref (since it hits first)
                        ((get-symbol info 'avoid-ref)
                         (append (list (format #f "o->~a" cname)) cargslst))
                        ((and (not setter?) (get-symbol info 'ref))
                         `(& ,(append (list (format #f "o->~a" cname)) cargslst)))
                        (else
                         (format #t "-- is infix? ~s ~s~%" info (memq 'infix info))
                         (if (memq 'infix info)
                             (format #f "*o ~a *~a" cname cargsstr)
                             (append (list (format #f "o->~a" cname)) cargslst))))))
        (if (null? opt-args)
            (list cmethtype methexpr)
            (list cmethtype
                  (cons 'cond ; emit required and optional args
                        (map (lambda (i)
                               `((== numArgs ,(+ (length req-args) i))
                                 ,(append (list (format #f "o->~a" cname))
                                          (form-callargs-max cargs (+ (length req-args) i)))))
                             (iota (+ 1 (length opt-args)))))))))
    (define (emit-code-return2 rets)
      (let* ((methtype (if rets (assq-ref rets 'type) #f))
             (gootype (if (and rets (assq-ref rets 'elm)) (assq-ref rets 'elm) #f))
             (gootype (cond
                       ((not gootype) 0)
                       ((string? gootype) gootype)
                       (else (format #f "GOO_TYPE::~a" gootype)))))
        (if (eq? 'sym methtype) (set! methtype (assq-ref rets 'ctype))) ; FIX, destructure rets properly
        (if rets
            (case (if rets (assq-ref rets 'type) #f)
              ((QString)
               '(s7_make_string sc (strdup (qPrintable r))))
              ((sym)
               `(let ((t "const char*" ,(format #f "~a_to_string(r)" methtype)))
                  (s7_make_symbol sc t)))
              ((int)  '(s7_make_integer sc r))
              ((real) '(s7_make_real sc r))
              ((bool) '(s7_make_boolean sc r))
              (else
               (emit-return-goo "r" (format #f "static_cast<uint64_t>(~a)" gootype))))
            '(s7_t sc))))
    (define (emit-method-nil methname)
      (let ((sname (string->symbol (format #f "ms-~a-~a" name methname)))
            (gootype 0)
            (methexpr (format #f "o->~a()" methname)))
        (defcompile
         (defcreg (sname) (1)
          '(raw "// ---- emit-method-get")
          (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
            `(raw ,(format #f "~a" methexpr))
            '(return (s7_t sc)))))))
    (define (get-method-sname-get methpair)
      (let ((methname (if (pair? methpair) (car methpair) methpair)))
        (string->symbol (format #f "ms-~a-~a" name methname))))
    (define (emit-method-get methpair args methargs)
      (let* ((cargs args) ; arguments to c-function
             (rets (if (pair? methargs) (car methargs) #f)) ; return-type of c-function
             (info (if (pair? (cdr methargs)) (cdr methargs) '()))
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair)))
       (format #t "~%emit-method-get ~s [~s ~s] ~s => ~s~%" methpair sname cname cargs rets)
       (defcompile
         (defcreg (sname) ((+ 1 (length cargs)))
           (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
             '(raw "// ---- emit-method-get")
             (emit-code-get-args2 'i 1 cargs
              (list
               (let* ((rets (compile-args 'r (list rets)))
                      (callinfo (emit-code-funcall4 #f info cname cargs '() (car rets)))
                      (cmethtype (car callinfo))
                      (methexpr (cadr callinfo)))
                 (if (assq-ref (car rets) 'stack) ; stack-allocated return
                   `(let ((tsa ,cmethtype ,methexpr)
                          (r ,(format #f "~a*" cmethtype) ,(format #f "new ~a(tsa)" cmethtype)))
                      ,(emit-code-return2 (car rets)))
                   `(let ((r ,cmethtype ,methexpr))
                      ,(emit-code-return2 (car rets))))))))))))
    (define (emit-method-set methpair cargs methargs)
      (let* ((info (if (pair? methargs) (cdr methargs) '()))
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair)))
        (format #t "~%emit-method-set ~s [~s ~s] ~s~%" methpair sname cname cargs)
        (defcompile
          (defcreg (sname) ((+ 1 (length cargs)))
            (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
              '(raw "// ---- emit-method-set")
              (emit-code-get-args2 'i 1 cargs
               (list
                (let* ((cargsstr (make-callargs cargs))
                       (methexpr (cond
                                  ((get-string info) (get-string info))
                                  ; this avoids constructing an ref (since it hits first)
                                  ((get-symbol info 'avoid-ref) (format #f "o->~a(~a)" cname cargsstr))
                                  ((get-symbol info 'ref) (format #f "&(o->~a(~a))" cname cargsstr))
                                  (else
                                   (if (memq 'infix info)
                                       (format #f "*o ~a *~a" cname cargsstr)
                                       (format #f "o->~a(~a)" cname cargsstr))))))
                  methexpr)
                's)))))))
    (define (emit-method-get/set methname cargs methargs)
      (format #t "emit-method-get/set: ~s methargs: ~s~%" methname methargs)
      (format #t "  -- ~s~%" (cdr methargs))
      (let* ((rets (car cargs)) ; get return type
             (info (if (pair? (cdr methargs)) (cdr methargs) '()))
             (methname-set (let ((s (symbol->string methname)))
                             (format #f "set~a~a"
                                     (string-upcase (substring s 0 1))
                                     (substring s 1))))
             (sname-get (get-method-sname-get methname))
             (sname-set (string->symbol (format #f "ms-~a-~a" name methname-set)))
             (cname-get (string->symbol (format #f "ms_~a_~a" name methname)))
             (cname-set (string->symbol (format #f "ms_~a_~a" name methname-set))))
        (format #t "info: ~s~%" info)
        (set! %export-to-scheme-getset2 (cons (list sname-get cname-get sname-set cname-set)
                                              %export-to-scheme-getset2))
        ; emit getter
        (defcompile
          (defcreg (sname-get) ((+ 1 (length cargs)))
             (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
              (if (assq-ref rets 'stack) ; stack-allocated return
                  (error "interim stack alloc not supported")
                  (let* ((callinfo (emit-code-funcall4 #f info methname '() '() rets))
                         (cmethtype (car callinfo))
                         (methexpr (cadr callinfo)))
                    `(let ((r ,cmethtype ,methexpr))
                       ,(emit-code-return2 rets)))))))
        ; emit setter
        (defcompile
          (defcreg (sname-set) ((+ 1 (length cargs)))
             (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
              (emit-code-get-args2 'i 1 cargs
               (list
                  (let* ((callinfo (emit-code-funcall4 #t info methname-set cargs '() rets))
                         (cmethtype (car callinfo))
                         (methexpr (cadr callinfo)))
                    methexpr)
                's)))))))
    (define (emit-method-apply methpair args methargs)
      (format #t "emit-method-apply ~s args: ~s flags: ~s~%" methpair args methargs)
      (let* ((info methargs)
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair))
             (required-args (make-required-args args))
             (optional-args (make-optional-args args))
             (len-required-args (length required-args))
             (len-optional-args (length optional-args)))
        (defcompile
          (defcreg (sname) ((+ 1 len-required-args) len-optional-args)
             (emit-pop-arg-goo2 #t #f #f #f `(,(format #f "~a*" c-type) "o") "g"
              (if (> (+ len-required-args len-optional-args) 0)
                  '(set! args (s7_cdr args)) '())
              (emit-code-get-argso #f 'i #f 1 required-args optional-args
               (list
                (let* ((callinfo (emit-code-funcall4 #f info cname required-args optional-args '()))
                       (cmethtype (car callinfo))
                       (methexpr (cadr callinfo)))
                  methexpr)
                (emit-code-return2 #f))))))))
    (for-each (lambda (meth)
                (match1 meth
                 ((typeof methname . methargs)
                  (format #t "emit method ~s of type: ~s~%" methname typeof)
                  (let ((args (if (pair? methargs)
                                  (if (eq? 'get/set typeof)
                                      (compile-args 's (list (car methargs)))
                                      (compile-args 's (car methargs)))
                                  '())))
                    (case typeof
                      ((nil)
                       (emit-method-nil methname))
                      ((set)
                       (emit-method-set methname args (cdr methargs)))
                      ((get)
                       ; methargs :: (arg ...) ret-type . flags
                       (emit-method-get methname args (cdr methargs)))
                      ((get/set) ; takes a single argument
                       (emit-method-get/set methname args methargs))
                      ((apply)
                       (emit-method-apply methname args (cdr methargs))))))))
              methods)))

(define (emit-registered-objects)
  (for-each emit-registered-object %objects))

; generate a .h and .c file, write to these files
(define %h (p-open-output-file "s7gen.h" "w"))
(define %c (p-open-output-file "s7gen.cpp" "w"))

(define (emit-string-to-ctype)
  (defcompile
    `(defcfun ("string_to_ctype") ("const char *sname") "int"
       ,@(apply append (map (lambda (lst)
                         (let ((typename (car lst))
                               (types (cdr lst)))
                           (map (lambda (type)
                                  (match type
                                    ((sname cname idx props)
                                     `(if (! (strcmp sname ,(format #f "\"~s-~s\"" typename sname)))
                                           (return ,(format #f "(int)~a" cname))))))
                                types)))
                       %c-types))
       -999999)))

(define (maybe-emit-args-check)
  (cond
   ((not (assis? 'argsPairKnow %compile-env))
    (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
    '(if (!(s7_is_pair args)) (return (s7_f sc))))
   (else '(noop))))

(define (emit-next-arg . args)
  ; resets checked-if-end-of-list flag
  (set! %compile-env (assq-set! %compile-env 'argsPairKnow #f))
  '(set! args (s7_cdr args)))

(define (emit-pop-arg-goo2 check-args? emit-next-arg? g? s? args cvar . body)
  (let ((type (car args))
        (name (cadr args)))
    (if g? (set! %compile-env (assq-set! %compile-env 'g g?)))
    (if s? (set! %compile-env (assq-set! %compile-env 's s?)))
    (append '(begin)
       (if emit-next-arg? (list (emit-next-arg)) '())
       '((raw "// ---- emit-pop-arg-goo ----"))
       (if check-args? '((if (!(s7_is_pair args)) (return (s7_f sc)))) '())
       (list
       (append
        (if s?
            `(begin (set! s (s7_car args)))
            `(let ((s s7_pointer (s7_car args)))))
        '((if (!(c_is_goo sc s)) (return (s7_f sc))))
        (list
         (append
          (if g?
              `(begin (set! ,(format #f "~a" cvar) "(goo_t *)s7_c_object_value(s)"))
              `(let ((,(format #f "~a" cvar) "goo_t*" ,(format #f "(goo_t *)s7_c_object_value(s)")))))
          (if (and type name)
              `((raw ,(format #f "    ~a ~a = (~a) ~a->cd"
                              type name type cvar)))
              '())
          body)))))))

(define (emit-maybe-next-arg)
  '(if (&& moreArgs (s7_is_pair args))
       (set! args (s7_cdr args))
       (set! moreArgs false)))

(define (emit-return-goo varname init)
  `(let ((ty uint64_t ,(format #f "(uint64_t) ~a" init)))
     (c_make_goo sc ty (s7_f sc) ,(format #f "(void*) ~a" varname))))

(define (c-ify-name sname)
  (let ((str "")
        (sname (if (string? sname) sname (symbol->string sname))))
    (do ((i 0 (+ 1 i)))
        ((>= i (string-length sname)))
      (set! str (format #f "~a~a" str
                        (let ((s (substring sname i (+ i 1))))
                          (if (string=? s "-") "_" s)))))
    str))

(define (emit-setenv args e)
  (let ((key (car args))
        (val (cadr args)))
    (assq-set! e key val)))

(define (emit-getenv e key)
  (cdr (assoc key e)))

(define (def-goo-setters-goo objtype objname getname setname operandtype)
  (let ((sname-getname (string->symbol (format #f "ms-~a-~a" objname getname)))
        (sname-setname (string->symbol (format #f "ms-~a-~a" objname setname))))
    (defcompile
      (defcreg (sname-getname) (1)
        (emit-pop-arg-goo2 #t #f #f #f `(,objtype "o") "g"
          `(let ((y ,operandtype (,(format #f "o->~a" getname))))
             ,(emit-return-goo "y" 0)))))
    (defcompile
      (defcreg (sname-setname) (2)
        (emit-pop-arg-goo2 #t #f #f #f `(,objtype "o") "go"
          (emit-pop-arg-goo2 #t #t #f #t `(,operandtype "x") "gx"
            `(,(format #f "o->~a" setname) x)
            '(s7_t sc)))))))

(define-syntax def-goo-setters-bool
  (syntax-rules ()
    ((k objtype objname memname memnameget memnameset)
     (begin
       (set! %export-to-scheme-getset (cons (list objname memname memnameset)
                                            %export-to-scheme-getset))
       (defcompile
         `(defcfun (,(format #f "ms_~a_~a" objname memname))
                   ("s7_scheme *sc" "s7_pointer args")
                   "s7_pointer"
            (let ((g "goo_t*" "(goo_t *)s7_c_object_value(s7_car(args))"))
              (let ((o ,(format #f "~a*" objtype) ,(format #f "(~a*) g->cd" objtype)))
                (s7_make_boolean sc (,(format #f "o->~a" memnameget)))))))

       (defcompile
         `(defcfun (,(format #f "ms_set_~a_~a" objname memname))
                   ("s7_scheme *sc" "s7_pointer args")
                   "s7_pointer"
            (let ((g "goo_t*" "(goo_t *)s7_c_object_value(s7_car(args))"))
              (let ((o ,(format #f "~a*" objtype) ,(format #f "(~a*) g->cd" objtype)))
                (let ((x "s7_pointer" (s7_cadr args)))
                  (,(format #f "o->~a" memnameset) (s7_boolean sc x))
                  x)))))))))

; FIX: the transname should be a symbol that references into %c-types-info
(define-syntax def-goo-setters-sym
  (syntax-rules ()
    ((k objtype objname memname memnameset transname)
     (begin
       (set! %export-to-scheme-getset (cons (list objname memname memnameset)
                                            %export-to-scheme-getset))
       (defcompile
         `(defcfun (,(format #f "ms_~a_~a" objname memname))
                   ("s7_scheme *sc" "s7_pointer args")
                   "s7_pointer"
            (let ((g "goo_t*" "(goo_t *)s7_c_object_value(s7_car(args))"))
              (let ((o ,(format #f "~a*" objtype) ,(format #f "(~a*) g->cd" objtype)))
                (s7_make_symbol sc (,(format #f "~a_to_string" transname) (,(format #f "o->~a"memname))))))))
       (if memnameset
          (defcompile
           `(defcfun (,(format #f "ms_set_~a_~a" objname memname))
                     ("s7_scheme *sc" "s7_pointer args")
                     "s7_pointer"
              (let ((g "goo_t*" "(goo_t *)s7_c_object_value(s7_car(args))"))
                (let ((o ,(format #f "~a*" objtype) ,(format #f "(~a*) g->cd" objtype)))
                  (let ((x "s7_pointer" "s7_cadr(args)"))
                    (,(format #f "o->~a" memnameset) (,(format #f "string_to_~a" transname) (s7_symbol_name x)))
                    x))))))))))

(define (emit-c-type-string-maps-simple name typelst basetypename)
  ; emit c-function that takes a object of a specific type and returns a string
  (defcompile
    `(defcfun (,(format #f "~a_to_string" name))
              (,(format #f "~a x" basetypename))
              "const char*"
       ,(append '(switch (x))
                (map (lambda (tri)
                       (list 'case (format #f "Ms::~a" (cadr tri))
                             (format #f "\"~a-~a\"" basetypename (cadr tri))))
                     typelst))))
  ; emit c-function that takes an string and returns an object of a specific type
  (defcompile
   `(defcfun (,(format #f "string_to_~a" name))
             ("const char *name")
             ,basetypename
      ,@(map (lambda (tri)
               `(if (!(strcmp name ,(format #f "\"~a\"" (cadr tri))))
                    (return ,(format #f "Ms::~a" (cadr tri)))))
             typelst)
      ,(format #f "(~a)0" basetypename))))

(define (emit-c-type-string-maps3 typename)
  (let* ((types (assq-ref %c-types typename))
         (typeinfo (assq-ref %c-types-info typename))
         (c-type (typeinfo-meta-get typename 'c-type-cons))
         (sname (string->symbol (format #f "~a-to-string" typename))))
    ;(set! c-type (substring c-type 0 (string-position ":" c-type)))
    ; emit c-function that takes a object of a specific type and returns a string
    (evalc
      `(defcfun (,sname) (,(format #f "Ms::~a x" c-type)) "const char*"
         ,(if (eq? c-type (typeinfo-meta-get typename 'c-type)) ; not an special-enum-class
              (append
                '(switch (x))
                (append
                 (map (lambda (lst)
                        (match lst
                          ((sname cname idx props)
                           (list 'case cname
                                 (format #f "\"~a-~a\"" typename sname)))))
                      types)
                 (list '(default "\"\""))))
              `,@(append
               (map (lambda (lst)
                      (match lst
                        ((sname cname idx props)
                         `(if (== x ,cname)
                              (return ,(format #f "\"~a\"" cname))))))
                    types)
               (list "\"ERROR-UNKNOWN\"")))))
    ; emit c-function that takes an string and returns an object of a specific type
    (let ((sname (string->symbol (format #f "string_to_~a" typename))))
      (evalc
       (append
          `(defcfun (,sname) ("const char *name") ,(format #f "Ms::~a" c-type))
          (map (lambda (lst)
                   (match lst
                     ((sname cname idx props)
                      `(if (!(strcmp name ,(format #f "\"~a-~a\"" typename sname)))
                           (return ,cname)))))
                 types)
           (list (format #f "(Ms::~a)0" c-type)))))))

(define (emit-exports-getset)
  (for-each (lambda (lst)
    (match lst
      ((sname-get cname-get sname-set cname-set)
       (format %c "s7_define_variable(sc, \"~a\", s7_dilambda(sc, \"~a\", ~a, 1, 0, ~a, 2, 0, \"\"));~%"
        sname-get sname-get cname-get cname-set))))
  %export-to-scheme-getset2))

(define (emit-exports)
  (format #t "Exporting ~a functions.~%" (length %export-to-scheme2))
  (for-each (lambda (lst)
    (match lst
      ((sname cname req-args opt-args)
       (format %c "s7_define_function(sc, \"~a\", ~a, ~a, ~a, false, \"(~a)\");~%" sname cname req-args opt-args sname))))
    %export-to-scheme2))

(define (gen-done) 
  (format #t "closing output port~%")
  (close-output-port %h)
  (close-output-port %c))
