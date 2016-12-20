#lang racket
(provide maak-adt-spel)
(require "teken-adt.rkt")
(require "Abstracties.rkt")
(require "Alien.rkt")
(require "Vloot.rkt")
(require "Schip.rkt")
(require "Kogel.rkt")
(require "Kogels-adt.rkt")
(require "Menu.rkt")
(require "rotator.rkt")
(require "howto.rkt")
(require "Score.rkt")
(require "Power-Ups.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;
;; SPEL ADT ;;
;;;;;;;;;;;;;;
;Dit adt zal de spellus bevatten. Ook zal het ervoor zorgen dat de input van het toetsenbord doorgegeven wordt aan de nodige spelelementen. Verder zal de collision detection ook in dit ADT gebeuren.

(define (maak-adt-spel)
  ;Maakt alle nodige adt's aan.
  (define teken-adt (maak-adt-teken "Space Invaders" px-venster-hoogte px-venster-breedte))
  (define schip-adt (maak-schip-adt))
  (define vloot-adt (maak-vloot-adt teken-adt))
  (define kogels-adt (maak-kogels-adt))
  (define menu-adt (maak-menu-adt))
  (define rotator-adt (maak-rotator-adt))
  (define how-to-adt (maak-how-to-adt))
  (define score-adt (maak-score-adt))
  (define Power-up-adt (maak-power-up-adt (/ (random 1 10) 10) (/ (random 5 10) 10) teken-adt))
  (define level 1)
  (define begin-aliens 10) ;aantal aliens die er in het 1e level moeten komen
  (define spel-tijd 0)
  (define start-positie-aliens 0.05)

  (define (pauze!) ;alles wat moet gebeuren als het spel gepauzeerd wordt
    ((menu-adt 'install!) teken-adt) ;verwijdert het schip, de kogels en de aliens en tekent het menu
    ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze) ;zet de correcte lussen
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze)
    ((rotator-adt 'teken!) teken-adt) ;tekent de rotator, een 1e keer
    ((menu-adt 'teken!) teken-adt) ;tekent het menu, een 1e keer
    )

  (define (restart!) ; bereidt het spel voor om te herstarten
    ((vloot-adt 'verwijder-vloot!))
    (pauze!) ; zet automatisch alles op pauze
    ((teken-adt 'reset-vloot!)) ; verwijdert de volledige vloot

    ((vloot-adt 'maak-aliens!) begin-aliens start-positie-aliens) ; maakt een nieuwe vloot aan
    (set! level 1) ; zet het level terug op 1
    )

  (define (play) ; start het spel op
    ((menu-adt 'uninstall!) teken-adt) ;tekent de sprites terug
    ;((teken-adt 'delete-menu!)) ; verwijdert het menu en de rotator
    ((teken-adt 'set-toets-functie!) toets-naar-spel-play) ;zet de correcte lussen
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-play)
    )

  (define (howto) ; gaat naar het howto menu
    ;((menu-adt 'uninstall!) teken-adt) ;vewijdert het menu
    ((how-to-adt 'install!) teken-adt); maakt de correcte tiles aan
    ((teken-adt 'set-toets-functie!) toets-naar-spel-howto) ;zet de correcte lus
    ((teken-adt 'teken-how-to!) how-to-adt) ; tekent de juiste tiles
    )
  (define (nieuw-level!) ; maakt een nieuw level aan
    ;((Power-up-adt 'delete!) teken-adt)
    ;((Power-up-adt 'reset!))
    (set! level (+ level 1)) ;verhoogt het level
    ((vloot-adt 'maak-aliens!) (* level begin-aliens) start-positie-aliens) ; maakt nieuwe en meerdere aliens aan
    ((vloot-adt 'maak-alien-tiles!)) ;tekent de tiles voor de nieuwe aliens
    )

  ;spellus
  ;Leest de toetsenbordinput uit en stuurt dit door naar het spel.
  ;Afhankelijk van de staat van het spel
  (define (toets-naar-spel-play toets)
    (cond
      ((eq? toets 'right)
       ((schip-adt 'beweeg!) 'rechts))
      ((eq? toets 'left)
       ((schip-adt 'beweeg!) 'links))
      ((eq? toets #\space)
       ((kogels-adt 'maak-kogel!) (schip-adt 'x) teken-adt))
      ((eq? toets #\p)
       (pauze!))))
  (define (toets-naar-spel-pauze toets); pas menu-tekenen na move
    (cond
      ((eq? toets #\return)
       (cond ((= 0 (rotator-adt 'staat))
              (play))
             ((= 1 (rotator-adt 'staat))
              (howto))
             ((= 2 (rotator-adt 'staat))
              (exit))))
      ((eq? toets 'up)
       ((rotator-adt 'up!) teken-adt))
      ((eq? toets 'down)
       ((rotator-adt 'down!) teken-adt))
      ))
  (define (toets-naar-spel-howto toets)
    (cond ((eq? toets #\return)
           (begin
             ;((menu-adt 'install!) teken-adt)
             ((how-to-adt 'uninstall!) teken-adt)
             ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze)
             ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze))
           )
          ))


  ; Functies voor het starten van het spel
  (define (start)
    ((menu-adt 'install!) teken-adt)
    ;Is de selector om in het menu te kiezen wat we willen openen
    (set! spel-tijd 0)
    ((vloot-adt 'maak-aliens!) begin-aliens start-positie-aliens)
    ((menu-adt 'teken!) teken-adt)
    ((rotator-adt 'teken!) teken-adt))

  ;;Schietlus, moet enkel opgeroepen worden als er geschoten is
  ;; Oproepen om het spel te laten lopen
  (define (spel-lus-functie-play delta-tijd)
    (set! spel-tijd (+ spel-tijd delta-tijd))
    ((schip-adt 'teken!) teken-adt)
    ((vloot-adt 'teken!) teken-adt)
    ((vloot-adt 'beweeg!) dispatch-spel)
    ;Indien de kogel afgeschoten is wordt de collision detection gedaan. Is performanter op deze manier, anders moet er elke beurt voor elke alien gecheckt worden.
    ((kogels-adt 'beweeg!) teken-adt vloot-adt score-adt Power-up-adt spel-tijd dispatch-spel)
    ((kogels-adt 'teken!) teken-adt)
    ((score-adt 'teken!) teken-adt)
    (if (eq? 'actief (Power-up-adt 'staat)) ;als het al actief is, moet het getekend worden, of na 5s van het scherm verdwijnen
        (if (< spel-tijd (+ timer5s (Power-up-adt 'random-nummer)))
            ((Power-up-adt 'teken!) teken-adt)
            (begin ((Power-up-adt 'delete!) teken-adt)
                   ((Power-up-adt 'reset!))))
        (if (> spel-tijd (Power-up-adt 'random-nummer)) ;als het op inactief staat moet het op de goede moment getekend worden
            ((Power-up-adt 'activeer!))
            'ok)
        )
    (if (and (eq? 'bezig (Power-up-adt 'staat))
             (> spel-tijd (+ timer5s (Power-up-adt 'timer))))
        ((Power-up-adt 'zet-terug!) vloot-adt kogels-adt)
        'ok); met de timer zien of het groter is
    (if (null? (vloot-adt 'vloot))
        (nieuw-level!)
        'ok))
  (define (spel-lus-functie-pauze delta-tijd)
    ; Input voor in het menu
    'ok
    )

  ;(nu worden de adt's zoals het moet in een lus hertekend)
  ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze)
  ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze)

  ;; Dispatch functie
  (define (dispatch-spel msg)
    (cond
      ((eq? msg 'start) start)
      ((eq? msg 'pauze!) pauze!)
      ((eq? msg 'restart!) restart!)
      ((eq? msg 'nieuw-level!) nieuw-level!)
      ))
  dispatch-spel)

;Maakt het ADT van het spel, moet wel nog opgeroepen worden.
(define spel (maak-adt-spel))

;Entry point om alles te laden
((spel 'start))
;https://docs.racket-lang.org/reference/strings.html
;
