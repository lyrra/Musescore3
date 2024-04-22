;
; test framework
;

(define-syntax deftest
  (syntax-rules ()
    ((deftest name form expected-output-h expected-output-c)
     (run-test name
               (lambda () form)
               expected-output-h
               expected-output-c))))

(define (run-test name test expected-output-h
                            expected-output-c)
  (define (report-fail error-name got expected)
    (format #t "~%Test FAIL:~%")
    (format #t "Got:~%")
    (format #t "------------------------------~%")
    (format #t "Pretty-printed:~%")
    (format #t "~a~%" got)
    (format #t "Raw:~%")
    (format #t "~s~%" got)
    (format #t "------------------------------~%")
    (format #t "Expected:~%")
    (format #t "~a~%" expected)
    (error error-name))
  (let* ((strc "")
         (strh (call-with-output-string
               (lambda (ph)
                 (set! strc
                       (call-with-output-string
                         (lambda (pc)
                           (let ((oldh %h)
                                 (oldc %c))
                             (set! %h ph)
                             (set! %c pc)
                             (test)
                             (set! %h oldh)
                             (set! %c oldc)))))))))
    (cond
     ((not (equal? strh expected-output-h))
      (report-fail (format #f "~a ~a" 'test-fail-h name)
                   strh expected-output-h))
     ((not (equal? strc expected-output-c))
      (report-fail (format #f "~a ~a" 'test-fail-c name)
                   strc expected-output-c))
     (else
      (if %verbose
        (format #t "PASS: ~s~%" name))))))

;
; test cases
;

(deftest 'simple_cfun
  (evalc
   '(defcfun (s-funname c_funname) ; scheme/C function name
             ("const char *arg1" "int arg2" "void* arg3") ; function arguments
             "int" ; return type of C function
      (if (! (strcmp arg1 "\"foo\""))
        (return 123))
      -999999))
  "int s_funname(const char *arg1, int arg2, void* arg3);\n"
  "\nint s_funname(const char *arg1, int arg2, void* arg3)\n{\n  if (!(strcmp(arg1, \"foo\"))) {\n  return 123;\n  }\n  return -999999;\n}\n\n")

(deftest 'simple_ffi_cfun
  (evalc
    `(defcreg (c_fun_name) ()
       (some_c_fun some-arg 0 (s7_f sc) nullptr)))
  "s7_pointer c_fun_name (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer c_fun_name (s7_scheme *sc, s7_pointer args)\n{\n  return some_c_fun(some-arg, 0, s7_f(sc), nullptr);\n}\n\n")

(deftest 'switch
  (evalc
   `(defcfun (f-switch) (1) "const char*"
      (switch (x)
        (case FOO "\"foo\"")
        (case Bar 1)
        (default 2))))
  "const char* f_switch(1);\n"
  "\nconst char* f_switch(1)\n{\nswitch (x) {\n  case FOO:\n  return \"foo\";\n  case Bar:\n  return 1;\n  default:\n    return 2;\n}\n;\n}\n\n")

(deftest 'enum
  (evalc
    `(defcenum GOO_TYPE "uint64_t"
               ,(append '()
                        (map (lambda (type)
                               (symbol->string type))
                             '(a b c)))))
  "enum class GOO_TYPE : uint64_t {\n  NIL = 0,\n  a,\n  b,\n  c,\n  END\n};\n"
  "")

(deftest 'pop-arg-goo
  (evalc
   `(defcreg (f-pop-arg-goo) (1)
      (x)
      ,(pop-arg-goo '("Void*" "arg") '())
      (y)))
  "s7_pointer f_pop_arg_goo (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_pop_arg_goo (s7_scheme *sc, s7_pointer args)\n{\nx();\n      // pop-arg-goo (Void* arg)\n;\n      a();\n      pop_arg();\n      b();\n;\n  return y();\n}\n\n")

(deftest 'pop-arg-goo-env
  (evalc
   `(defcreg (f-pop-arg-goo-env) (1)
      (x)
      ,(pop-arg-goo '("Void*" "arg") '()) ; modifies environment
      ,(pop-arg-goo '("Void*" "arg") '((pop-arg . #t))) ; pop_arg() call should be pruned if env is set
      (y)))
  "s7_pointer f_pop_arg_goo_env (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_pop_arg_goo_env (s7_scheme *sc, s7_pointer args)\n{\nx();\n      // pop-arg-goo (Void* arg)\n;\n      a();\n      pop_arg();\n      b();\n;\n      // pop-arg-goo (Void* arg)\n;\n      a();\n    ;\n      b();\n;\n  return y();\n}\n\n")

(deftest 'defcreg-raw
  (evalc
   `(defcreg (f_raw) (1)
      (raw "const char *symname = s7_symbol_name(s7_car(args))")
      (raw "Element* e = NULL")
      ,(append
        '(cond)
        (map (lambda (type)
               (let ((name (car type))
                     (cname (cadr type)))
                 (list `(! (strcasecmp symname ,(format #f "\"~a\"" name)))
                       (list 'raw (format #f "e = Element::create(~a, g_mtest->score)" cname))
                       (list 'raw (format #f "static_cast<uint64_t>(GOO_TYPE::~a)"
                                          (symbol->string name))))))
             '((A "a") (B b)))
        (list
         (list 'else
               '(raw "error(\"ERROR: UNRECOGNIZED ELEMENT TYPE\")")
               '(return (s7_nil sc)))))))
  "s7_pointer f_raw (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_raw (s7_scheme *sc, s7_pointer args)\n{\n  const char *symname = s7_symbol_name(s7_car(args));\n  Element* e = NULL;\n  if (!(strcasecmp(symname, \"A\"))) {\n      e = Element::create(a, g_mtest->score);\n      return   static_cast<uint64_t>(GOO_TYPE::A);\n  } else if (!(strcasecmp(symname, \"B\"))) {\n      e = Element::create(b, g_mtest->score);\n      return   static_cast<uint64_t>(GOO_TYPE::B);\n  } else {\n      error(\"ERROR: UNRECOGNIZED ELEMENT TYPE\");\n      return   return s7_nil(sc);\n  }\n;\n}\n\n")

(deftest 'defcreg-begin-1
  (evalc
   `(defcreg (f-begin-1) (1)
      (begin (a))))
  "s7_pointer f_begin_1 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_begin_1 (s7_scheme *sc, s7_pointer args)\n{\n      return a();\n;\n}\n\n")

(deftest 'defcreg-begin-2
  (evalc
   `(defcreg (f-begin-2) (1)
      (begin (a) (b))))
  "s7_pointer f_begin_2 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_begin_2 (s7_scheme *sc, s7_pointer args)\n{\n    a();\n      return b();\n;\n}\n\n")

(deftest 'defcreg-if-1
  (evalc
   `(defcreg (f-if-1) (1)
      (if (f) (a))
      (c)))
  "s7_pointer f_if_1 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_if_1 (s7_scheme *sc, s7_pointer args)\n{\n  if (f()) {\na();\n  }\n  return c();\n}\n\n")


(deftest 'defcreg-if-2
  (evalc
   `(defcreg (f-if-2) (1)
      (if (f) (a) (b))
      (c)))
  "s7_pointer f_if_2 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_if_2 (s7_scheme *sc, s7_pointer args)\n{\n  if (f()) {\na();\n  } else {\nb();\n  }\n  return c();\n}\n\n")

(deftest 'defcreg-if-1-tcp
  (evalc
   `(defcreg (f-if-1-tcp) (1)
      (if (f) (a))))
  "s7_pointer f_if_1_tcp (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_if_1_tcp (s7_scheme *sc, s7_pointer args)\n{\n  if (f()) {\n  return a();\n  }\n}\n\n")

(deftest 'defcreg-if-2-tcp
  (evalc
   `(defcreg (f-if-2-tcp) (1)
      (if (f) (a) (b))))
  "s7_pointer f_if_2_tcp (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_if_2_tcp (s7_scheme *sc, s7_pointer args)\n{\n  if (f()) {\n  return a();\n  } else {\n  return b();\n  }\n}\n\n")

(deftest 'defcreg-when-1
  (evalc
   `(defcreg (f-when-1) (1)
      (when (f) (a))
      (c)))
  "s7_pointer f_when_1 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_when_1 (s7_scheme *sc, s7_pointer args)\n{\nif (f()) {\n    a();\n}\n;\n  return c();\n}\n\n")

(deftest 'defcreg-when-2
  (evalc
   `(defcreg (f-when-2) (1)
      (when (f) (a) (b))
      (c)))
  "s7_pointer f_when_2 (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_when_2 (s7_scheme *sc, s7_pointer args)\n{\nif (f()) {\n    a();\n    b();\n}\n;\n  return c();\n}\n\n")

(deftest 'defcreg-when-1-tcp
  (evalc
   `(defcreg (f-when-1-tcp) (1)
      (when (f) (a))))
  "s7_pointer f_when_1_tcp (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_when_1_tcp (s7_scheme *sc, s7_pointer args)\n{\nif (f()) {\n      return a();\n}\n;\n}\n\n")

(deftest 'defcreg-when-2-tcp
  (evalc
   `(defcreg (f-when-2-tcp) (1)
      (when (f) (a) (b))))
  "s7_pointer f_when_2_tcp (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_when_2_tcp (s7_scheme *sc, s7_pointer args)\n{\nif (f()) {\n    a();\n      return b();\n}\n;\n}\n\n")

(deftest 'defcreg-if-infix-andand
  (evalc
   `(defcreg (f_infix_andand) ()
      (if (&& (c1) (c2)) (a))))
  "s7_pointer f_infix_andand (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_infix_andand (s7_scheme *sc, s7_pointer args)\n{\n  if (c1() && c2()) {\n  return a();\n  }\n}\n\n")

(deftest 'defcreg-nextarg
  (evalc
   `(defcreg (f-nextarg) (1 2)
      ,(next-arg)
      (s7_t sc)))
  "s7_pointer f_nextarg (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_nextarg (s7_scheme *sc, s7_pointer args)\n{\n  args = s7_cdr(args);\n  return s7_t(sc);\n}\n\n")

; ensure we can return a value from cond in tail-call-position
; ensure usage of multiple statements in cond
(deftest 'defcreg-cond-return
  (evalc
   `(defcreg (f_cond_return) ()
      (cond
       ((c1)
        (a)
        (s7_t sc))
       (else
        (s7_f sc)))))
  "s7_pointer f_cond_return (s7_scheme *sc, s7_pointer args);\n"
  "s7_pointer f_cond_return (s7_scheme *sc, s7_pointer args)\n{\n  if (c1()) {\n    a();\n      return s7_t(sc);\n  } else {\n      return s7_f(sc);\n  }\n;\n}\n\n")
