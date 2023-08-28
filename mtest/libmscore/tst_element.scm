
(define (tst-element elm-name)
  (let* ((e  (ms-make-element elm-name))
         (g2 (ms-mtest-writeReadElement e)))
    (if (= (ms-element-type e) (ms-element-type g2))
        (ms-test-check-pass)
        (ms-test-check-fail))))

(for-each (lambda (elm)
  (let ((name (car elm))
        (cname (cadr elm))
        (idx   (caddr elm))
        (props (cadddr elm)))
   (when (and (memq 'create props)
              ; avoid these two types which causes a crash
              (not (eq? 'ElementType-BRACKET name))
              (not (eq? 'ElementType-NOTELINE name)))
     (tst-element name))))
  (cdr (assoc 'ElementType %c-types)))
