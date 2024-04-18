;
; test framework
;

(define-syntax deftest
  (syntax-rules ()
    ((deftest name form expected-output)
     (run-test name
               (lambda () form)
               expected-output))))

(define (run-test name test expected-output)
  (let ((str
         (with-output-to-string
           (lambda ()
             (let ((old-c %c))
               (set! %c (current-output-port))
               (test)
               (set! %c old-c))))))
    (when (not (equal? str expected-output))
      (format #t "~%Test FAIL:~%")
      (format #t "Got:~%")
      (format #t "------------------------------~%")
      (format #t "Pretty-printed:~%")
      (format #t "~a~%" str)
      (format #t "Raw:~%")
      (format #t "~s~%" str)
      (format #t "------------------------------~%")
      (format #t "Expected:~%")
      (format #t "~a~%" expected-output)
      (error 'test-fail name))))

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
  "\nint s_funname(const char *arg1, int arg2, void* arg3)\n{\nif (!(strcmp(arg1, \"foo\"))) {\n  return 123;\n}\n  return -999999;\n}\n\n")

(deftest 'simple_ffi_cfun
  (evalc
    `(defcreg (c_fun_name) ()
       (some_c_fun some-arg 0 (s7_f sc) nullptr)))
  "s7_pointer c_fun_name (s7_scheme *sc, s7_pointer args)\n{\n  return some_c_fun(some-arg, 0, s7_f(sc), nullptr);\n}\n\n")

(deftest 'maps3
  (let* ((typename "Foo")
         (types '((s-1 CONST_1) (s-2 CONST_2)))
         (c-type "FooType")
         (sname (string->symbol (format #f "to-string"))))
    (evalc
      `(defcfun (,sname) (,(format #f "Ms::~a x" c-type)) "const char*"
         ,(append
          '(switch (x))
          (append
           (map (lambda (lst)
                  (match lst
                    ((sname cname )
                     (list 'case cname
                           (list 'return (format #f "\"~a-~a\"" typename sname))))))
                types)
           (list '(default (return "\"\"")))))))

    ;----- 
    (evalc
      `(defcfun (,sname) (,(format #f "Ms::~a x" c-type)) "const char*"
         ,@(append
           (map (lambda (lst)
                  (match lst
                    ((sname cname)
                     `(if (== x ,cname)
                          (return ,(format #f "\"~a\"" cname))))))
                types)
               (list "\"ERROR-UNKNOWN\""))))
    ;----- 

    ; emit c-function that takes an string and returns an object of a specific type
    (let ((sname (string->symbol (format #f "string_to_~a" typename))))
      (evalc
       (append
          `(defcfun (,sname) ("const char *name") ,(format #f "Ms::~a" c-type))
          (map (lambda (lst)
                   (match lst
                     ((sname cname )
                      `(if ,(format #f "!strcmp(name, \"~a-~a\")" typename sname)
                           (return ,cname)))))
                 types)
           (list (format #f "(Ms::~a)0" c-type))))))
  "\nconst char* to_string(Ms::FooType x)\n{\nswitch (x) {\n  case CONST_1:\n  return \"Foo-s-1\";\n  case CONST_2:\n  return \"Foo-s-2\";\n  default:\n    return \"\";\n\n}\n}\n\n\nconst char* to_string(Ms::FooType x)\n{\nif (x == CONST_1) {\n  return \"CONST_1\";\n}\nif (x == CONST_2) {\n  return \"CONST_2\";\n}\n  return \"ERROR-UNKNOWN\";\n}\n\n\nMs::FooType string_to_Foo(const char *name)\n{\nif (!strcmp(name, \"Foo-s-1\")) {\n  return CONST_1;\n}\nif (!strcmp(name, \"Foo-s-2\")) {\n  return CONST_2;\n}\n  return (Ms::FooType)0;\n}\n\n")

