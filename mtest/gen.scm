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

(define (compile-register-cfun sname num-req-args num-opt-args body)
  (format #t "compile-register-cfun sname=~s  num-req-args=~s  snum-opt-args=~s~%"
          sname num-req-args num-opt-args)
  (let ((cname (c-ify-name sname)))
    (set! %export-to-scheme2
          (cons (list sname cname num-req-args num-opt-args)
                %export-to-scheme2))
    (append `(defcfun (,sname ,cname)
               ("s7_scheme *sc" "s7_pointer args")
               "s7_pointer")
            body)))

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
                            (list . body)))))

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

(define-syntax emit-stat-fmt
  (syntax-rules ()
    ((_ . args)
     (format #f . args))))

(define (emit-registered-object pair)
  (let* ((name (car pair))
         (args (cdr pair))
         (c-type (assq-ref args 'c-type))
         (make-args (assq-ref args 'make-args))
         (methods (assq-ref args 'methods)))
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
    (define (make-callargs-max args numargs)
      (let ((n 0) (str ""))
        (for-each (lambda (arg)
                    (set! n (+ 1 n))
                    (if (<= n numargs)
                          (set! str (format #f "~a~a~ax~a"
                                            str (if (= n 1) "" ", ")
                                            (if (memq 'deref arg) "*" "") ; c-ref(&), need to ref pointer
                                            n))))
                  args)
        str))
    (define (make-optional-args args)
      (filter (lambda (arg)
                (and (pair? arg) (pair? (cdr arg)) (pair? (cddr arg))
                     (eq? 'opt (caddr arg))))
              args))
    (define (make-required-args args)
      (filter (lambda (arg)
                (not (and (pair? arg) (pair? (cdr arg)) (pair? (cddr arg))
                          (eq? 'opt (caddr arg)))))
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
    (define (emit-code-get-args required-args optional-args)
      (append
       (list 'begin '(raw "// ---- emit-code-get-args ----"))
      (let ((n 0))
        (cond
         ((null? optional-args)
          (map (lambda (arg)
                 (set! n (+ n 1))
                 (let ((type (car arg)))
                   (list 'begin
                    (emit-next-arg)
                    (cond
                     ((string? type) ; goo type
                      (emit-pop-arg-goo `(,(if (memq 'stack arg) (format #f "~a*" type) type)
                                          ,(format #f "x~a" n))))
                     ((eq? 'bool type) (emit-pop-arg-bool (list (format #f "x~a" n))))
                     ((eq? 'int  type) (emit-pop-arg-int  (list (format #f "x~a" n))))
                     ((eq? 'real type) (emit-pop-arg-real (list (format #f "x~a" n))))
                     ((eq? 'sym type)
                      (let ((conv (cadr arg))
                            (stype (typeinfo-meta-get (cadr arg) 'c-type-cons)))
                        (list 'begin
                         (emit-pop-arg-sym (list (format #f "t~a" n)))
                         (list 'raw (format #f "Ms::~a x~a = string_to_~a(t~a)" stype n conv n)))))
                     (else (error "unknown arg-type" type))))))
               required-args))
         (else
           (append
           ; emit all required arguments
           (map (lambda (arg)
                  (set! n (+ n 1))
                  (list 'begin
                   (emit-next-arg)
                   (case (car arg)
                     ((int) (emit-pop-arg-int (list (format #f "x~a" n))))
                     ((sym)
                      (let ((stype (cadr arg)))
                        (list 'begin
                         (emit-pop-arg-sym (list (format #f "t~a" n)))
                         (list 'raw (format #f "Ms::~a x~a = string_to_~a(t~a)" stype n stype n)))))
                     (else (emit-pop-arg-goo (list (car arg) (format #f "x~a" n)))))))
                required-args)
           (list (if (> (length optional-args) 0)
                     (list 'raw (format #f "bool moreArgs = true"))
                     (emit-noop)))
           (list (list 'raw (format #f "int numArgs = ~a" (length required-args))))
           ; emit optional arguments
           (map (lambda (arg)
                  (set! n (+ n 1))
                  (list 'begin
                   (emit-maybe-next-arg)
                   (case (car arg)
                     ((int) (emit-pop-arg-int (list (format #f "x~a" n) #t)))
                     ((sym)
                      (let ((stype (cadr arg)))
                        (list 'begin
                         (emit-pop-arg-sym (list (format #f "t~a" n) #t))
                         (list 'raw (format #f "Ms::~a x~a = string_to_~a(t~a)" stype n stype n)))))
                     (else (emit-pop-arg-goo (list (car arg) (format #f "x~a" n) #f #t))))))
                optional-args)))))))
    (define (emit-code-funcall info cname cargs required-args optional-args rets)
      (if (null? optional-args)
          (let* ((methtype (cond
                            ((not rets) #f)
                            ((string? (car rets)) (car rets))
                            ((= (length rets) 2)
                             (typeinfo-meta-get (cadr rets) 'c-type-cons))
                            (else (car rets))))
                 (cmethtype (case methtype
                              ((real) 'double)
                              (else methtype)))
                 (cargsstr (make-callargs cargs))
                 (methexpr (cond
                            ((get-string info) (get-string info))
                            ; this avoids constructing an ref (since it hits first)
                            ((get-symbol info 'avoid-ref) (format #f "o->~a(~a)" cname cargsstr))
                            ((get-symbol info 'ref) (format #f "&(o->~a(~a))" cname cargsstr))
                            (else
                             (if (memq 'infix info)
                                 (format #f "*o ~a *~a" cname cargsstr)
                                 (format #f "o->~a(~a)" cname cargsstr))))))
            (cond
             ((not methtype) ; dont care about return value
              (emit-stat-fmt "~a" methexpr))
             ((memq 'stack rets) ; stack-allocated return
              (list 'begin
               (list 'raw (format #f "~a tsa = ~a /* stack-alloc */" cmethtype methexpr))
               ; would be nicer to just do malloc(sizeof(...)), but we might need to run constructors
               ; and also c++ knows how to move an compound object
               (list 'raw (format #f "~a* r = new ~a(tsa)" cmethtype cmethtype))))
             (else
              (emit-stat-fmt "~a r = ~a" cmethtype methexpr))))
          (list ; emit required and optional args
           (let ((lst '()))
             (do ((i (length required-args) (+ i 1)))
                 ((> i (+ (length required-args) (length optional-args))))
               (set! lst (cons (list
                                (emit-stat-fmt "if (numArgs == ~a) {" i)
                                (emit-stat-fmt "~ao->~a(~a)"
                                                     (if rets (format #f "~a r = " (car rets)) "")
                                                     cname
                                                     (make-callargs-max cargs i))
                                (emit-stat-fmt "}"))
                               lst)))
             lst))))
    (define (emit-code-return rets)
      (let* ((methtype (if rets (car rets) #f))
             (gootype (if (and rets (list-nth rets 2)) (list-nth rets 2) #f))
             (gootype (cond
                       ((not gootype) 0)
                       ((string? gootype) gootype)
                       (else (format #f "GOO_TYPE::~a" gootype)))))
        (if (eq? 'sym methtype) (set! methtype (cadr rets))) ; FIX, destructure rets properly
        (if rets
            (case (if rets (list-nth rets 0) #f)
              ((QString)
               '(emit-stat "return s7_make_string(sc, strdup(qPrintable(r)))"))
              ((sym)
               (list (emit-stat-fmt "const char* t = ~a_to_string(r)" methtype)
                     '(emit-stat "return s7_make_symbol(sc, t)")))
              ((int)  '(emit-stat "return s7_make_integer(sc, r)"))
              ((real) '(emit-stat "return s7_make_real(sc, r)"))
              ((bool) '(emit-stat "return s7_make_boolean(sc, r)"))
              (else
               (emit-return-goo "r" (format #f "static_cast<uint64_t>(~a)" gootype))))
            '(emit-stat "return s7_t(sc)"))))
    (define (emit-method-nil methname methargs)
      (let ((sname (string->symbol (format #f "ms-~a-~a" name methname)))
            (gootype 0)
            (methexpr (if (null? methargs)
                        (format #f "o->~a()" methname)
                        (car methargs))))
        (emit-cfun `(,sname) 1 (list
          `(emit-pop-arg-goo (,(format #f "~a*" c-type) "o"))
          (lambda (e) (format %c "
    ~a;
    return s7_t(sc);~%" methexpr))))))
    (define (get-method-sname-get methpair)
      (let ((methname (if (pair? methpair) (car methpair) methpair)))
        (string->symbol (format #f "ms-~a-~a" name methname))))
    (define (emit-method-get methpair methargs)
      (let* ((cargs (car methargs)) ; arguments to c-function
             (rets (if (pair? (cdr methargs)) (cadr methargs) #f)) ; return-type of c-function
             (info (if (pair? (cdr methargs)) (cddr methargs) #f))
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair)))
       (format #t "~%emit-method-get ~s [~s ~s] ~s~%" methpair sname cname cargs)
       (defcompile
         (defcreg (sname) ((+ 1 (length cargs)))
           (emit-pop-arg-goo `(,(format #f "~a*" c-type) "o"))
           (emit-code-get-args cargs '())
           (emit-code-funcall info cname cargs '() '() rets)
           (emit-code-return rets)))))
    (define (emit-method-set methpair methargs)
      (format #t "emit-method-set ~s~%" methpair)
      (let* ((cargs (car methargs)) ; arguments to c-function
             (info (if (pair? (cdr methargs)) (cddr methargs) '()))
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair)))
        (defcompile
          (defcreg (sname) ((+ 1 (length cargs)))
            (emit-pop-arg-goo `(,(format #f "~a*" c-type) "o"))
            (emit-code-get-args cargs '())
            (emit-code-funcall info cname cargs '() '() #f)
            's))))
    (define (emit-method-get/set methname methargs)
      (let* ((rets (car methargs))
             (cargs (list rets))
             (info (if (pair? (cdr methargs)) (cdr methargs) '()))
             (cmethtype (case (car rets)
                          ((real) 'double)
                          ((sym) (typeinfo-meta-get (cadr rets) 'c-type-cons))
                          ((goo) (cadr rets))
                          (else (car rets))))
             (methname-set (let ((s (symbol->string methname)))
                             (format #f "set~a~a"
                                     (string-upcase (substring s 0 1))
                                     (substring s 1))))
             (sname-get (get-method-sname-get methname))
             (sname-set (string->symbol (format #f "ms-~a-~a" name methname-set)))
             (cname-get (string->symbol (format #f "ms_~a_~a" name methname)))
             (cname-set (string->symbol (format #f "ms_~a_~a" name methname-set))))
        (format #t "emit-method-get/set x1~%")
        (set! %export-to-scheme-getset2 (cons (list sname-get cname-get sname-set cname-set)
                                              %export-to-scheme-getset2))
        ; emit getter
        (defcompile
          (defcreg (sname-get) ((+ 1 (length cargs)))
             (emit-pop-arg-goo `(,(format #f "~a*" c-type) "o"))
             (emit-code-funcall info methname '() '() '() rets)
             (emit-code-return rets)))
        (format #t "emit-method-get/set x2~%")
        ; emit setter
        (defcompile
          (defcreg (sname-set) ((+ 1 (length cargs)))
             (emit-pop-arg-goo `(,(format #f "~a*" c-type) "o"))
             (emit-code-get-args cargs '())
             ; by tacking on avoid-ref, we mean that the function should not be referenced
             (emit-code-funcall (cons 'avoid-ref info) methname-set
                                 ; if the c-function parameter is a reference
                                 ; we need to dereference the arguments (it is a pointer [we only deal in pointers])
                                 (if (memq 'ref info)
                                     (map (lambda (arg) (cons 'deref arg)) cargs)
                                     cargs)
                                 '() '() #f)
             's))))
    (define (emit-method-apply methpair methargs)
      (format #t "emit-method-apply ~s~%" methpair)
      (let* ((cargs (car methargs))
             (info (cdr methargs))
             (cname (if (pair? methpair) (cadr methpair) methpair))
             (sname (get-method-sname-get methpair))
             (required-args (make-required-args cargs))
             (optional-args (make-optional-args cargs))
             (len-required-args (length required-args))
             (len-optional-args (length optional-args)))
        (defcompile
          (defcreg (sname) ((+ 1 len-required-args) len-optional-args)
             ;`((req-args . ,(+ 1 (length required-args)))
             ;(opt-args . ,(length optional-args)))
             (emit-pop-arg-goo `(,(format #f "~a*" c-type) "o"))
             (emit-code-get-args required-args optional-args)
             (emit-code-funcall info cname cargs required-args optional-args #f)
             (emit-code-return #f)))))
    (for-each (lambda (meth)
                (match1 meth
                 ((typeof methname . methargs)
                  (format #t "emit method ~s of type: ~s~%" methname typeof)
                  (case typeof
                   ((nil)     (emit-method-nil methname methargs))
                   ((set)     (emit-method-set methname methargs))
                   ((get)     (emit-method-get methname methargs))
                   ((get/set) (emit-method-get/set methname methargs))
                   ((apply)   (emit-method-apply methname methargs))))))
              methods)))

(define (emit-registered-objects)
  (for-each emit-registered-object %objects))

; generate a .h and .c file, write to these files
(define %h (open-output-file "s7gen.h" "w"))
(define %c (open-output-file "s7gen.cpp" "w"))

(define (emit-string-to-ctype)
  (format %h "int string_to_ctype (const char *sname);~%")
  (format %c "
int string_to_ctype (const char *sname)
{
")
  (map (lambda (lst)
         (let ((typename (car lst))
               (types (cdr lst)))
           (map (lambda (lst)
                  (match lst
                    ((sname cname idx props)
                     (format %c "    if (!strcmp(sname, \"~s-~s\")) {~%" typename sname)
                     (format %c "        return (int)~a;~%" cname)
                     (format %c "    }~%"))))
                types)))
       %c-types)
  (format %c "    return -999999;    ~%}~%"))

(define (emit-header-getset objname memname)
  (format %h "s7_pointer ms_~a_~a (s7_scheme *sc, s7_pointer args);~%" objname memname)
  (format %h "s7_pointer ms_set_~a_~a (s7_scheme *sc, s7_pointer args);~%" objname memname))

; write the beginning of a c-function. FIX: (with-c-fun (args) body) would be better
(define (emit-func-prolog setter objname memname objtype)
  (format %c "
s7_pointer ms_~a~a_~a (s7_scheme *sc, s7_pointer args)
{
    goo_t *g = (goo_t *)s7_c_object_value(s7_car(args));
    ~a* o = (~a*) g->cd;
" (if setter "set_" "") objname memname objtype objtype))

(define (emit-noop args e) e)

(define (maybe-emit-args-check)
  (cond
   ((not (assis? 'argsPairKnow %compile-env))
    (set! %compile-env (assq-set! %compile-env 'argsPairKnow #t))
    '(if (!(s7_is_pair args)) (return (s7_f sc))))
   (else '(noop))))

(define-syntax def-emit-pop-arg
  (lambda (x)
    (define (nfo typ)
      (case typ
        ((int)  '(emit-pop-arg-int  integer "int" "s7_integer(s)"))
        ((sym)  '(emit-pop-arg-sym  symbol "const char*" "s7_symbol_name(s)"))
        ((bool) '(emit-pop-arg-bool boolean "bool" "s7_boolean(sc, s)"))
        ((real) '(emit-pop-arg-real real "double" "s7_real(s)"))
        ((goo)  '(emit-pop-arg-goo  goo "goo_t*"))
        (else
         (error "unknown type (to nfo)" typ))))
    (syntax-case x ()
     ((def-emit-pop-arg type)
      (let* ((y (syntax->datum (syntax type)))
             (lst (nfo y)))
        (with-syntax ((fname (datum->syntax (syntax def-emit-pop-arg) (car lst)))
                      (stype (datum->syntax (syntax def-emit-pop-arg) (cadr lst)))
                      (ctype (datum->syntax (syntax def-emit-pop-arg) (caddr lst)))
                      (sconv (datum->syntax (syntax def-emit-pop-arg) (cadddr lst))))
          (syntax
          (define (fname args)
  (let ((cargname (car args))
        (optional (and (pair? (cdr args)) (cadr args)))
        (s (assis? 's %compile-env)))
    (set! %compile-env (assq-set! %compile-env 's #t))
    (cond
     (optional
      `(begin
         (raw ,(format #f "~a ~a" ctype cargname))
         (when moreArgs
           ,(maybe-emit-args-check)
           (raw "numArgs++")
           (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
           (if (! (,(string->symbol (format #f "s7_is_~a" 'stype)) s)) (return (s7_f sc)))
           (raw ,(format #f "~a = ~a;~%" cargname sconv)))))
     (else
      `(begin
         ,(maybe-emit-args-check)
         (raw ,(format #f "~as = s7_car(args)" (if s "" "s7_pointer ")))
         (if (! (,(string->symbol (format #f "s7_is_~a" 'stype)) s)) (return (s7_f sc)))
         (raw ,(format #f "~a ~a = ~a;~%" ctype cargname sconv))))))))))))))

(def-emit-pop-arg sym)
(def-emit-pop-arg int)
(def-emit-pop-arg bool)
(def-emit-pop-arg real)

(define (emit-pop-arg-goo args . rest)
  (let ((type (if (pair? args) (car args) #f))
        (name (if (and (pair? args) (pair? (cdr args))) (cadr args) #f))
        (cvar (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args))) (caddr args) "g"))
        (cvarg (and (pair? args) (pair? (cdr args)) (pair? (cddr args))))
        (optional (list-nth args 3)))
    (let ((cur-g (assis? 'g %compile-env))
          (cur-s (assis? 's %compile-env)))
      (set! %compile-env (assq-set! %compile-env 's #t))
      (unless cvarg
        (set! %compile-env (assq-set! %compile-env 'g #t)))
      (cond
       (optional
        `(begin
           (raw "// ---- emit-pop-arg-goo ----")
           (raw ,(format #f "    ~a ~a" (if (and (not cvarg) cur-g) "" "goo_t* ") cvar))
           ,(if (and type name)
               `(raw ,(format #f "  ~a ~a" type name))
               '(noop))
           (when moreArgs
             ,(maybe-emit-args-check)
             (raw "        numArgs++")
             (raw ,(format #f "        ~as = s7_car(args);~%" cur-s))
             (if (!(c_is_goo sc s)) (return (s7_f sc)))
             (raw ,(format #f "        ~a~a = (goo_t *)s7_c_object_value(s)"
                           (if (and (not cvarg) cur-g) "" "goo_t* ") cvar))
             ,(if (and type name)
                  `(raw ,(format #f "    ~a = (~a) ~a->cd" name type cvar))
                  '(noop)))))
       (else
        `(begin
           (raw "// ---- emit-pop-arg-goo ----")
           ,(maybe-emit-args-check)
           (raw ,(format #f" ~as = s7_car(args)" (if cur-s "" "s7_pointer ")))
           (if (!(c_is_goo sc s)) (return (s7_f sc)))
           (raw ,(format #f "~a~a = (goo_t *)s7_c_object_value(s)"
                         (if (and (not cvarg) cur-g) "" "goo_t* ") cvar))
           ,(if (and type name)
               `(raw ,(format #f "    ~a ~a = (~a) ~a->cd"
                              type name type cvar))
               '(noop))))))))

(define (emit-next-arg . args)
  ; resets checked-if-end-of-list flag
  (set! %compile-env (assq-set! %compile-env 'argsPairKnow #f))
  '(raw  "args = s7_cdr(args)"))

(define (emit-maybe-next-arg)
  '(if (&& moreArgs (s7_is_pair args))
       (raw "args = s7_cdr(args)")
       (raw "moreArgs = false")))

(define (emit-return-goo varname init)
  `(begin
     (raw ,(format #f "uint64_t ty = (uint64_t) ~a" init))
     (c_make_goo sc ty (s7_f sc) ,(format #f "(void*) ~a" varname))))

(define (emit-stat args e)
  (format %c "    ~a;~%" (car args))
  e)

(define (c-ify-name sname)
  (let ((str "")
        (sname (symbol->string sname)))
    (do ((i 0 (+ 1 i)))
        ((>= i (string-length sname)))
      (set! str (format #f "~a~a" str
                        (let ((s (substring sname i (+ i 1))))
                          (if (string=? s "-") "_" s)))))
    str))

(define (emit-inst-eval-form form e)
  (cond
   ((null? form))
   ((pair? form)
    (let ((fun-name (car form)))
      ((symbol->value fun-name) (cdr form) e)
      ))
   (else (form e))))

(define (emit-inst-eval forms e)
  (when (pair? forms)
    (let ((form (car forms)))
      (emit-inst-eval (cdr forms)
                      (emit-inst-eval-form form e)))))

(define (emit-cfun-eval sname cname arity body)
  (define (flatten-inst lst)
    (let ((new '()))
      (define (iter form)
        (cond
         ((and (pair? form) (symbol? (car form)))
          (set! new (cons form new)))
         ((pair? form)
          (for-each iter form))
         (else
          (set! new (cons form new)))))
      (iter lst)
      (reverse new)))
  (let ((body (flatten-inst body)))
    (emit-inst-eval (append (list '(emit-prolog)) body)
                    `((sname . ,sname) (cname . ,cname) (arity . ,arity)
                      (s . #f) (g . #f)))))

(define (emit-cfun names args body)
  (let* ((req-args (if (and (pair? args) (pair? (car args)))
                       (assq-ref args 'req-args) #f))
         (opt-args (if (and (pair? args) (pair? (car args)))
                       (assq-ref args 'opt-args) #f))
         (arity (if (pair? args) (car args) args))
         (register (not (and (pair? args) (eq? 'no-register (cadr args)))))
         (sname (car names))
         (cname (if (null? (cdr names)) #f (cadr names)))
         (cname (or cname (c-ify-name sname))))
    (if register
      (set! %export-to-scheme2
            (cons (list sname cname (or req-args arity) (or opt-args 0))
                  %export-to-scheme2)))
    (format %h "s7_pointer ~a (s7_scheme *sc, s7_pointer args);~%" cname)
    (format %c "s7_pointer ~a (s7_scheme *sc, s7_pointer args)~%{~%" cname)
    (if (pair? body)
      (emit-cfun-eval sname cname arity body)
      (body))
    (format %c "}~%~%")))

(define (emit-setenv args e)
  (let ((key (car args))
        (val (cadr args)))
    (assq-set! e key val)))

(define (emit-getenv e key)
  (cdr (assoc key e)))

(define (emit-prolog args e)
  (unless (assis? 'c-prolog e)
    (let ((arity (assoc 'arity e)))
      (if arity (set! arity (cdr arity)))
      ;(if (and arity (> arity 0)) ; args are used, emit check that ensures they are provided
      ;  (format %c "    if (!s7_is_pair(args)) return s7_f(sc);~%"))
      (set! e (cons (cons 'c-prolog #t) e))))
  e)

(define (def-goo-setters-goo objtype objname getname setname operandtype)
  (let ((sname-getname (string->symbol (format #f "ms-~a-~a" objname getname)))
        (sname-setname (string->symbol (format #f "ms-~a-~a" objname setname))))
    (defcompile
      (defcreg (sname-getname) (1)
         (emit-pop-arg-goo `(,objtype "o"))
         `(raw ,(format #f "   ~a y = o->~a();~%" operandtype getname))
         (emit-return-goo "y" 0)))
    (defcompile
      (defcreg (sname-setname) (2)
         (emit-pop-arg-goo `(,objtype "o" "go"))
         (emit-next-arg)
         (emit-pop-arg-goo `(,operandtype "x" "gx"))
         `(raw ,(format #f "    o->~a(x)" setname))
         '(s7_t sc)))))

(define-syntax def-goo-setters-bool
  (syntax-rules ()
    ((k objtype objname memname memnameget memnameset)
     (begin
       (set! %export-to-scheme-getset (cons (list objname memname memnameset)
                                            %export-to-scheme-getset))
       (emit-header-getset objname memname)
       (emit-func-prolog #f objname memname objtype)
       (format %c "
    return s7_make_boolean(sc, o->~a());
}
" memnameget)
       (emit-func-prolog #t objname memname objtype)
       (format %c "
  s7_pointer x = s7_cadr(args);
  o->~a(s7_boolean(sc, x));
  return x;
}
" memnameset)))))

; FIX: the transname should be a symbol that references into %c-types-info
(define-syntax def-goo-setters-sym
  (syntax-rules ()
    ((k objtype objname memname memnameset transname)
     (begin
       (set! %export-to-scheme-getset (cons (list objname memname memnameset)
                                            %export-to-scheme-getset))
       (emit-header-getset objname memname)
       (emit-func-prolog #f objname memname objtype)
       (format %c "
    return s7_make_symbol(sc, ~a_to_string(o->~a()));
}
" transname memname)
       (when memnameset
         (emit-func-prolog #t objname memname objtype)
         (format %c "
  s7_pointer x = s7_cadr(args);
  o->~a(string_to_~a(s7_symbol_name(x)));
  return x;
}
" memnameset transname))))))

(define (emit-c-type-string-maps name typelst basetypename)
  (let ()
    (format %h "const char* ~a_to_string (~a x);~%" name basetypename)
    (format %c "const char* ~a_to_string (~a x)~%" name basetypename)
    (format %c "{~%    switch (x) {~%")
    (map (lambda (lst)
                (match lst
                  ((symname c-type)
                   (format %c "        case ~a:~%" c-type)
                   (format %c "        return \"~s-~s\";~%" basetypename symname))))
         typelst)
    (format %c "    fprintf(stderr, \"WARNING: Unknown ctype-int %i\\n\", (int) c-type);~%")
    (format %c "    }~%}~%")

    (format %h "~a string_to_~a (const char *name);~%" basetypename name)
    (format %c "~a string_to_~a (const char *name)~%" basetypename name)
    (format %c "{~%~%")
    (map (lambda (lst)
                (match lst
                  ((symname c-type)
                   (format %c "    if (!strcmp(name, \"~s\")) {~%" symname)
                   (format %c "        return ~a;~%" c-type)
                   (format %c "    }~%"))))
         typelst)
    (format %c "    fprintf(stderr, \"WARNING: Unknown ctype-str %s\\n\", name);~%")
    (format %c "    return (~a)0;~%" basetypename)
    (format %c "}~%")))

(define (emit-c-type-string-maps-simple name typelst basetypename)
  (let ()
    ; emit c-function that takes a object of a specific type and returns a string
    (format %h "const char* ~a_to_string (~a x);~%" name basetypename)
    (format %c "const char* ~a_to_string (~a x)~%" name basetypename)
    (format %c "{~%    switch (x) {~%")
    (for-each (lambda (tri)
      (format %c "        case Ms::~a:~%" (cadr tri))
      (format %c "        return \"~a-~a\";~%" basetypename (cadr tri)))
      typelst)
    (format %c "    }~%}~%")
    ; emit c-function that takes an string and returns an object of a specific type
    (format %h "~a string_to_~a (const char *name);~%" basetypename name)
    (format %c "~a string_to_~a (const char *name)~%" basetypename name)
    (format %c "{~%~%")
    (for-each (lambda (tri)
      (format %c "    if (!strcmp(name, \"~a\")) {~%" (cadr tri))
      (format %c "        return Ms::~a;~%" (cadr tri))
      (format %c "    }~%"))
      typelst)
    (format %c "    return (~a)0;~%" basetypename)
    (format %c "}~%")))

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
                      `(if ,(format #f "!strcmp(name, \"~a-~a\")" typename sname)
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
