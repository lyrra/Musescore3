
(define %c-types '())
(define %c-types-info '())

(define (register-c-type lst)
  (let* ((start-index (or (assq-ref lst 'start-index) 0))
         (name        (assq-ref lst 'name))
         (c-type      (assq-ref lst 'c-type))
         (c-impltype  (assq-ref lst 'c-impltype))
         (types       (assq-ref lst 'types)))
    ; store information about the type
    (set! %c-types-info (cons (cons (cdr (assq 'name lst)) lst) %c-types-info))
    ; store the types values (enumeration)
    (let ((typelst (or (assq-ref %c-types name) '()))
          (idx start-index))
      (do ((pair types (cdr pair)))
          ((eq? '() pair))
        (let ((type-or-list (car pair))
              (type #f)
              (typename #f)
              (c-typename #f)
              (index #f))
          (cond
           ((pair? type-or-list)
            (set! type (car type-or-list))
            (cond
            ((and (pair? (cadr type-or-list)) (equal? 'or (car (cadr type-or-list))))
             (let* ((types (cdr (cadr type-or-list)))
                    (str (format #f "~a::~a" name (car types))))
               (do ((pair (cdr types) (cdr pair)))
                   ((null? pair))
                 (let ((tname (car pair)))
                   (format #f " ~a::~a | ~a" name tname str)))
               str))
            (else
              (set! index (cadr type-or-list)))))
           (else
            (set! type type-or-list)))
          (set! typename (string->symbol (format #f "~a-~a" name type)))
          (set! c-typename (format #f "~a::~a" c-type type))
          (set! typelst (cons (list typename c-typename idx) typelst))
          (set! idx (or index (+ idx 1)))))
      (if (assq-ref %c-types name)
        (let ((pair (assoc name %c-types)))
          (set-cdr! pair typelst))
        (set! %c-types (cons (cons name typelst) %c-types))))))

(define (typeinfo-meta-get name what)
  (let ((types (assq-ref %c-types-info name)))
    (if types
      (case what
        ((c-type-cons)
          (or (assq-ref types 'c-type-cons)
              (assq-ref types 'c-type)))
        ((c-type) (assq-ref types 'c-type))
        ((c-impltype)
          (or (assq-ref types 'c-impltype)
              (assq-ref types 'c-type)))
        (else
         (error "unknown request of type-info" what)))
      (error "type not found in type-info" name))))

(define (typeinfo-get name type what)
  (let ((types (assq-ref %c-types name)))
    (if types
      (let ((typelst (assq-ref types type)))
        (if typelst
          (cond
           ((eq? 'c-typename what) (car typelst))
           ((eq? 'index what) (cadr typelst)))
          f))
      #f)))

(define (eval-define-c-type args)
  (display args)
  (register-c-type args))

(define-syntax define-c-type
  (syntax-rules ()
    ((_ . args)
     (eval-define-c-type (car 'args)))))
