#lang racket
(provide maak-score-adt)
(require "teken-adt.rkt")
(require "Abstracties.rkt")

(define (maak-score-adt)
  (define score 0)
  (define positie-x 0)
  (define positie-y 0)

  (define (verhoog! aantal)
    (set! score (+ score aantal)))

  (define (teken! teken-adt)
    ((teken-adt 'teken-score!) dispatch-score))

  (define (dispatch-score msg)
    (cond ((eq? msg 'verhoog!) verhoog!)
          ((eq? msg 'score) score)
          ((eq? msg 'x) positie-x)
          ((eq? msg 'y) positie-y)
          ((eq? msg 'teken!) teken!)
          ))

  dispatch-score)
;http://docs.racket-lang.org/reference/file-ports.html?q=do
