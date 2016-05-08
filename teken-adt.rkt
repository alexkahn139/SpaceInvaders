#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "Graphics.rkt")
(require "helpers.rkt")
(require "Abstracties.rkt")
;Debug functies komen uit de helpers.rkt file die met het snakeproject meegegeven zijn. Deze werden tijdens het schrijven van de code veel gebruikt om zaken mee te testen.
;zodat ander files hieraan kunnen

(provide maak-adt-teken)
;Zorgt evoor dat het teken-adt vanuit de spelfile aangesproken kan worden.


;;;;;;;;;;;;;;;;;
;;Het teken-adt;;
;;;;;;;;;;;;;;;;;

;Maakt het teken-adt met een venster waarop getekend kan worden
(define (maak-adt-teken titel h-pixels v-pixels)
  (define venster (make-window v-pixels h-pixels titel))
  ((venster 'set-background!) "black")

  ;;;;;;;;;;
  ;;CONFIG;;
  ;;;;;;;;;;
  (define (redraw-all! lijst laag)
  (map (lambda (pair) ;neemt een assoclijst zoals de alien-tiles
     ((laag 'add-drawable) (cdr pair)))
   lijst))
(define (verwijder-alle! lijst laag)
(map (lambda (pair) ;neemt een assoclijst zoals de alien-tiles
   ((laag 'remove-drawable) (cdr pair)))
 lijst))
  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Config voor alien ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Maakt een alien-laag aan. Hierop zullen de alien(-tiles) getekend worden.
  (define alien-laag (venster 'make-layer))

  ;;; Beginnen met een lege lijst aan tiles, zodat we er in de globale omgeving aankunnen
  (define vloot-tiles '())

  ;;; Functie om alien-tiles aan de vloot toe te voegen.
  (define (voeg-alien-toe! alien-adt)
    (define kleurfoto '())
    (cond ((eq? 'geel (alien-adt 'kleur))
           (set! kleurfoto (make-bitmap-tile "monster-geel.jpg"))) ;moet met set! anders geen actie gedaan
          ((eq? 'groen (alien-adt 'kleur))
           (set! kleurfoto (make-bitmap-tile "monster-groen.jpg"))))
    (set! vloot-tiles (cons (cons alien-adt kleurfoto) vloot-tiles))
    ;((alien-laag 'add-drawable) kleurfoto)
    )
  (define (voeg-alien-tile-toe! alien-adt)
    (let* ((alien-tile (neem-iets alien-adt vloot-tiles)))
      ((alien-laag 'add-drawable) alien-tile)))
  ;;; Functie om de correcte alien of kogel uit de lijst te halen.
  (define (neem-iets iets-adt uit-tiles)
    (cdr (assoc iets-adt uit-tiles)))

  ;;; Functie om een alien te verwijderen. Zowel uit de lijst van de vloot-tiles als effectief van het scherm (remove-drawable)
  (define (verwijder-alien! alien-adt)
    ;(debug "delete-alien")
    (let* ((alien-tile (neem-iets alien-adt vloot-tiles)))
      ((alien-laag 'remove-drawable) alien-tile))
    (set! vloot-tiles (remove alien-adt vloot-tiles (lambda (r e) (eq? (car e) e)))))
  (define (reset-vloot!)
    (set! vloot-tiles '())
    (verwijder-alle! vloot-tiles alien-laag)
    )



  ;;;;;;;;;;;;;;;;;;;;;
  ;;Config voor schip;;
  ;;;;;;;;;;;;;;;;;;;;;

  ;;;De laag waarop het schip getekend zal worden
  (define schip-laag (venster 'make-layer))

  ;;; In dit geval is er slechts een tile dus die kan hardcoded staan
  (define schip-tile (make-bitmap-tile "schip.jpg" "schip-mask.jpg"))

  ;;; Zorgt ervoor dat het schip getekend kan worden. Moet niet te deleten zijn.
  (define (maak-schip!)
    ((schip-laag 'add-drawable) schip-tile))

  ;verwijderen van het schip
  (define (verwijder-schip!)
    ((schip-laag 'remove-drawable) schip-tile))


  ;;;;;;;;;;;;;;;;;;;;;
  ;;Config voor kogel;;
  ;;;;;;;;;;;;;;;;;;;;;
  ; Laag waarop de kogel getekend zal worden
  (define kogel-laag (venster 'make-layer))

  ; Een lege lijst voor de kogel, zo zal deze pas op het scherm komen nadat men hem nodig heeft.
  (define kogel-tiles '())

  ; Functie die de kogel aanmaakt en in assoc lijst steekt
  (define (maak-kogel! kogel-adt)
    (define kogel-obj '())
    (if (eq? 'normaal (kogel-adt 'type))
        (set! kogel-obj (make-bitmap-tile "kogel.jpg"))
        (set! kogel-obj (make-bitmap-tile "kogel-groen.jpg")))
    (set! kogel-tiles (cons (cons kogel-adt kogel-obj) kogel-tiles))
    ((kogel-laag 'add-drawable) kogel-obj)
    )

  ; Functie om een getekende kogel te verwijderen
  (define (verwijder-kogel! kogel-adt)
    (let ((kogel-obj (neem-iets kogel-adt kogel-tiles)))
      ((kogel-laag 'remove-drawable) kogel-obj))
    (set! kogel-tiles (remove kogel-adt kogel-tiles (lambda (r e) (eq? (car e) e)))))


  ;;;;;;;;;;;;;;;;;;;;
  ;;Config voor menu;;
  ;;;;;;;;;;;;;;;;;;;;
  ; Laag waarop het menu getekend zal worden
  (define menu-laag (venster 'make-layer))
  (define menu-tile (make-bitmap-tile "Menu.png"))

  ;Functie om het menu te verwijderen
  (define (verwijder-menu!)
    ((rotator-laag 'remove-drawable) rotator-tile)
    ((menu-laag 'remove-drawable) menu-tile)
    (display "verwijder menu")
    )

  ;Functie om alles wat weggehaald wordt bij het aanmaken van een menu te hertekenen.
  (define (herteken-alles!)
    (maak-schip!)
    (verwijder-menu!)
    (redraw-all! vloot-tiles alien-laag)
    ;(verwijder-menu!)
    (display "alles ok"))

  ;Functie om alles weg te halen bij het aanmaken van het menu. Zorgt ervoor dat dit ook al spelende kan gebeuren.
  (define (verwijder-alles!)
    ((menu-laag 'add-drawable) menu-tile)
    (verwijder-alle! vloot-tiles alien-laag)
    (verwijder-alle! kogel-tiles kogel-laag)
    (verwijder-schip!)

    ) ;tekent het menu

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;;Config voor rotator;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  ; Laag waarop de rotator getekend zal worden
  (define rotator-laag (venster 'make-layer))
  (define rotator-tile (make-bitmap-tile "rotator.jpg"))
  (define (maak-rotator)
    ((rotator-laag 'add-drawable) rotator-tile))
  (define (verwijder-rotator!)
    ((rotator-laag 'remove-drawable) rotator-tile))

  ;;;;;;;;;;;;;;;;;;;;;
  ;;Config voor howto;;
  ;;;;;;;;;;;;;;;;;;;;;

  ; Laag waarop het how-to scherm getekend zal worden
  (define how-to-laag (venster 'make-layer))
  (define how-to-tile '())

  (define (maak-how-to)
    (let ((tile (make-bitmap-tile "HowTo.jpg")))
      (set! how-to-tile tile)
      ((how-to-laag 'add-drawable) tile)))
  (define (verwijder-how-to!)
    ((how-to-laag 'remove-drawable) how-to-tile)
    (set! how-to-tile '()))
  ;;;;;;;;;;;;;;;;;;;;;
  ;;Config voor score;;
  ;;;;;;;;;;;;;;;;;;;;;

  ; Laag waarop het how-to scherm getekend zal worden
  (define score-laag (venster 'make-layer))
  (define score-tile (make-tile h-pixels v-pixels))
  ((score-laag 'add-drawable) score-tile)

  ; #TODO config voor Power-Ups
  (define Power-Up-laag (venster 'make-layer))
  (define Power-Up-tile (make-tile h-pixels v-pixels))
  ((Power-Up-laag 'add-drawable) Power-Up-tile)

  (define (delete-power-up!)
    ((Power-Up-laag 'remove-drawable) Power-Up-tile))
  ;;;;;;;;;;;;;;;;;;
  ;;TEKEN FUNCTIES;;
  ;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;
  ;;Schip;;
  ;;;;;;;;;

  (define (teken-schip! schip-adt)
    (let* ((schip-x (* h-pixels (schip-adt 'x)))
           (schip-y (- v-pixels px-schip-hoogte))
           (absolute-x (centraliseer schip-x 'schip)))
      ((schip-tile 'set-x!) absolute-x)
      ((schip-tile 'set-y!) schip-y)
      ;(debug "schip tekenen" schip-x " " schip-y)
      ))

  ;;;;;;;;;
  ;;Alien;;
  ;;;;;;;;;

  (define (teken-alien! alien-adt)
    (let* ((alien-x (* h-pixels (alien-adt 'x)))
           (alien-y (* v-pixels   (alien-adt 'y)))
           (alien-tile (neem-iets alien-adt vloot-tiles))
           (absolute-x (centraliseer alien-x 'alien)))
      ((alien-tile 'set-x!) absolute-x)
      ((alien-tile 'set-y!) alien-y)
      ;(debug "Alien tekenen" alien-x " " alien-y)
      ))

  ;;;;;;;;;
  ;;Kogel;;
  ;;;;;;;;;

  (define (teken-kogel! kogel-adt)
    (let* ((kogel-x (* h-pixels (kogel-adt 'x)))
           (kogel-y (* v-pixels (kogel-adt 'y)))
           (kogel-tile (neem-iets kogel-adt kogel-tiles))
           (absolute-x (centraliseer kogel-x 'kogel)))
      ((kogel-tile 'set-x!) absolute-x)
      ((kogel-tile 'set-y!) kogel-y)
      ;(debug "kogel tekenen" kogel-x " " kogel-y)
      ))

  ;;;;;;;;
  ;;Menu;;
  ;;;;;;;;

  (define (teken-menu! menu-adt)
    (let* ((start-x (* h-pixels (menu-adt 'x)))
           (start-y (* v-pixels (menu-adt 'y)))
           (absolute-x (centraliseer start-x 'start)))
      ((menu-tile 'set-x!) absolute-x)
      ((menu-tile 'set-y!)  start-y)
      ))

  ;;;;;;;;;;;
  ;;Rotator;;
  ;;;;;;;;;;;

  (define (teken-rotator! rotator-adt)
    (let* ((rotator-x (* h-pixels (rotator-adt 'x)))
           (rotator-y (* v-pixels (rotator-adt 'y)))
           (absolute-x (centraliseer rotator-x 'rotator)))
      ((rotator-tile 'set-x!) absolute-x)
      ((rotator-tile 'set-y!)  rotator-y)
      ))

  ;;;;;;;;;;
  ;;How-To;;
  ;;;;;;;;;;
  (define (teken-how-to! how-to-adt)
    (if (not (null? how-to-tile))
        (begin
          (let* ((how-to-x (* h-pixels (how-to-adt 'x)))
                 (how-to-y (* v-pixels (how-to-adt 'y)))
                 (absolute-x (centraliseer how-to-x 'how-to)))
            ((how-to-tile 'set-x!) how-to-x)
            ((how-to-tile 'set-y!)  how-to-y)
            )
          )
        'ok))

  ;;;;;;;;;
  ;;Score;;
  ;;;;;;;;;

  (define (teken-score! score-adt)
    (let* ((score-x (* h-pixels (score-adt 'x)))
           (score-y (* v-pixels (score-adt 'y)))
           (string (string-append "score: "(number->string (score-adt 'score)) "                   high-score: " (number->string (score-adt 'high-score))))

           )
      (score-tile 'clear)
      ((score-tile 'draw-text) string
                               20
                               score-x
                               score-y
                               "green")))

  ;TODO Tekenfunctie voor Power-Ups
  (define (teken-Power-UP! Power-Up-adt kleur)
    (let* ((Power-Up-x (* h-pixels (Power-Up-adt 'x)))
           (Power-Up-y (* v-pixels (Power-Up-adt 'y)))
           ;(absolute-x (centraliseer Power-Up-x 'Power-up))
           )
      (Power-Up-tile 'clear)
      ((Power-Up-tile 'draw-ellipse) Power-Up-x Power-Up-y px-alien-hoogte px-alien-breedte kleur)
      )
    )


  ;;;;;;;;;;;;;;;;;;;;;
  ;; Spellus functies;;
  ;;;;;;;;;;;;;;;;;;;;;

  (define (set-spel-lus-functie! fun)
    ((venster 'set-update-callback!) fun))

  (define (set-toets-functie! fun)
    ((venster 'set-key-callback!) fun))


  ;;;;;;;;;;;;;;
  ;; Dispatch ;;
  ;;;;;;;;;;;;;;

  (define (dispatch-teken-adt msg)
    (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
          ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
          ;; Teken functies.
          ((eq? msg 'teken-schip!) teken-schip!)
          ((eq? msg 'teken-alien!) teken-alien!)
          ((eq? msg 'teken-kogel!) teken-kogel!)
          ((eq? msg 'teken-menu!) teken-menu!)
          ((eq? msg 'teken-rotator!) teken-rotator!)
          ((eq? msg 'teken-how-to!) teken-how-to!)
          ((eq? msg 'teken-score!) teken-score!)
          ((eq? msg 'teken-power-up!) teken-Power-UP!)
          ;; Delete en maak functies
          ((eq? msg 'herteken-alles!) herteken-alles!)
          ((eq? msg 'nieuwe-alien!) voeg-alien-toe!)
          ((eq? msg 'nieuwe-kogel!) maak-kogel!)
          ((eq? msg 'maak-rotator) maak-rotator)
          ;((eq? msg 'maak-power-up-tile) maak-power-up-tile)
          ((eq? msg 'maak-how-to) maak-how-to)
          ((eq? msg 'delete-alien!) verwijder-alien!)
          ((eq? msg 'delete-kogel!) verwijder-kogel!)
          ((eq? msg 'delete-schip!) verwijder-schip!)
          ((eq? msg 'delete-menu!) verwijder-menu!)
          ((eq? msg 'delete-rotator!) verwijder-rotator!)
          ((eq? msg 'delete-how-to!) verwijder-how-to!)
          ((eq? msg 'delete-power-up!) delete-power-up!)
          ((eq? msg 'verwijder-alles) verwijder-alles!)
          ((eq? msg 'reset-vloot!) reset-vloot!)
          ((eq? msg 'voeg-alien-tile-toe!) voeg-alien-tile-toe!)
          (else (display "error, message ") (display msg) (display "not understood"))))
  dispatch-teken-adt)
