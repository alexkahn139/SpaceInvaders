#lang racket
(provide maak-score-adt)
(require "teken-adt.rkt")
(require "Abstracties.rkt")

(define (maak-score-adt)
  (define score 0)
  (define high-score 0)
  (define positie-x 0)
  (define positie-y 0)
  (define pad "score.txt")

  (define (lees-van-schijf!)
      (when (file-exists? pad) ;als het bestand niet bestaat wordt het ook niet opgeroepen
        (call-with-input-file pad
          (lambda (file)
            (set! high-score (read file))))))

  (define (opslaan!)
   (when (> score high-score) (begin
                                (save-scores-to-file!)
                                (set! high-score score))))

  (define (verhoog! aantal)
    (set! score (+ score aantal))
    (opslaan!))

  (define (teken! teken-adt)
    ((teken-adt 'teken-score!) dispatch-score))
    ;variabele bestand is eigenlijk een poort naar het bestand
  (define (save-scores-to-file!)
   (define bestand (open-output-file pad
                                    #:mode'binary
                                    #:exists'replace)) ;als het bestand als bestaat, wordt het gewoon overschreven
    (write high-score bestand)
    (close-output-port bestand))

  (define (dispatch-score msg)
    (cond ((eq? msg 'verhoog!) verhoog!)
          ((eq? msg 'score) score)
          ((eq? msg 'high-score) high-score)
          ((eq? msg 'x) positie-x)
          ((eq? msg 'y) positie-y)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'opslaan!) opslaan!)
          ))
  (lees-van-schijf!)
  dispatch-score)
;http://docs.racket-lang.org/reference/file-ports.html?q=do
