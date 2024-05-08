(define %source-dir #f)

(do ((pair %command-line (cdr pair)))
    ((null? pair))
  (let ((arg (car pair)))
    (let ((n (string-position "--srcdir=" arg)))
      (if n
        (set! %source-dir (substring arg (+ 9 n)))))))

(load (format #f "~a/s7/lib.scm" %source-dir))
(load (format #f "~a/mtest/ms.scm" %source-dir))
(load "types-code-gen.scm")

(define (check-write-read-elm elm fun val)
  (let ((e (ms-mtest-writeReadElement elm))) ; Elm e = static_cast<Elm*>(writeReadElement(elm))
    (if (number? val)
      ; QCOMPARE(e->pitch(), val) where fun: getter for note->pitch
      (ms-test-check (= val (fun e)))
      (ms-test-check (eq? val (fun e)))))) ; QCOMPARE(e->pitch(), val) where fun: getter for note->pitch
