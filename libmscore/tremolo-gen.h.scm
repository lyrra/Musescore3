
(define-c-type (
  (start-index . -1)
  (name        . TremoloType)
  (c-type      . "TremoloType")
  (datatype    . "signed char")
  (types       . (
    INVALID_TREMOLO
      R8  R16  R32  R64  BUZZ_ROLL ; one note tremolo (repeat)
      C8  C16  C32  C64            ; two note tremolo (change)
      ))))

(define-c-type (
  (name        . TremoloStyle)
  (c-type      . "TremoloStyle")
  (datatype    . "signed char")
  (types       . (
    DEFAULT TRADITIONAL TRADITIONAL_ALTERNATE))))
