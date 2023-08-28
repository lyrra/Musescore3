
; check subtype for hairpin
(let ((hp (ms-make-hairpin)))
  (for-each (lambda (type)
    (set! (ms-hairpin-hairpinType hp) type)
    (check-write-read-elm hp ms-hairpin-hairpinType type))
    '(hairpin-hairpin-DECRESC_HAIRPIN hairpin-hairpin-CRESC_HAIRPIN)))
