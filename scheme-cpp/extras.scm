
(define (assis? key lst)
  (let ((pair (assoc key lst)))
    (if pair
        (cdr pair)
        #f)))

(define (list-nth lst index)
  (if (> (length lst) index)
      (list-ref lst index)
      #f))
