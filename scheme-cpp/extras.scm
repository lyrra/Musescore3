
(define (assis? key lst)
  (let ((pair (assoc key lst)))
    (if pair
        (cdr pair)
        #f)))

(define (list-nth lst index)
  (if (> (length lst) index)
      (list-ref lst index)
      #f))

(define (%string-interleave inter lst cnt acc)
  (if (null? lst)
      acc
      (%string-interleave inter (cdr lst) (+ 1 cnt) (format #f "~a~a~a"
                                                            acc
                                                            (if (> cnt 0) inter "")
                                                            (car lst)))))

(define (string-interleave lst int)
  (%string-interleave int lst 0 ""))

