#lang racket
;Code van het snakeproject, werd letterlijk overgenomen om te debuggen
(provide debug)


(define debug? #t)

(define (debug x . xs)
  (when debug?
    (display x)
    (for-each display xs)
    (newline)))
