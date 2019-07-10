#lang racket
; Set up the state and lenses
; AndrewJ 2019-07-06

; Imports
(require racket/hash
         lens/common
         lens/data/hash
         threading
         text-table)

; Exports
(provide (all-defined-out))

; Composable versions of lens functions. Similar to PureScript lens library functions.
(define (view _lens x) (lens-view _lens x))
(define (at _lens x y) (lens-set _lens y x))
(define (over _lens f x) (lens-transform _lens x f))
(define >>> lens-thrush)
(define <<< lens-compose)

;-------------------------------
; Define card types
(define card-types '(Diamond Gold Silver Cloth Spice Leather Camel))

;-------------------------------
; Define the initial game state 
(define empty-hand (hash 'Diamond 0 'Gold 0 'Silver 0
                         'Cloth 0 'Spice 0 'Leather 0
                         'Camel 0))

(define all-cards (hash 'Diamond 6 'Gold 6 'Silver 6
                        'Cloth 8 'Spice 8 'Leather 10
                        'Camel 11))

(define initial-state
  (hash  'Deck all-cards
         'Market empty-hand
         'Hand (hash 'A empty-hand
                     'B empty-hand)
         'Points (hash 'A 0
                       'B 0)
         'Tokens (hash 'Diamond '(7 7 5 5 5)
                       'Gold '(6 6 5 5 5)
                       'Silver '(5 5 5 5 5)
                       'Cloth '(5 3 3 2 2 1 1)
                       'Spice '(5 3 3 2 2 1 1)
                       'Leather '(4 3 2 1 1 1 1 1 1))))

;-------------------------------
; Lenses into the state
; Borrowing the Purescript tradition, all lenses start with an underscore.
(define _deck (hash-ref-lens 'Deck))
(define _market (hash-ref-lens 'Market))
(define _hand (hash-ref-lens 'Hand))
(define _points (hash-ref-lens 'Points))
(define _tokens (hash-ref-lens 'Tokens))
(define (_rsrc r) (hash-ref-lens r))
(define (_player p) (hash-ref-lens p))

;-------------------------------
; Number of cards in a player's hand, excluding camels
(define (count-cards-excl-camels plyr st)
  (define total (foldl + 0 (hash-values (view (>>> _hand (_player plyr)) st))))
  (define camels (view (>>> _hand (_player plyr) (_rsrc 'Camel)) st))
  (- total camels))

; The minimum sell quantity of each card
(define min-sell-hash
  (hash 'Diamond 2 'Gold 2 'Silver 2
        'Cloth 1 'Spice 1 'Leather 1
        'Camel 99))

(define (min-sell k)
  (hash-ref min-sell-hash k))

;-------------------------------
; Pretty print the state

(define (list-cards title _lens st)
  (append (list title)
          (~>> st (view _lens) (hash-values))))

(define (ppst st)
  (displayln
   (table->string
    #:align 'center #:row-sep? #f
    (list (append '("") (~>> st (view _deck) (hash-keys))) ; header row
          (list-cards "Deck" _deck st)
          (list-cards "Market" _market st)
          (list-cards "Hand A" (>>> _hand (_player 'A)) st)
          (list-cards "Hand B" (>>> _hand (_player 'B)) st))))
  
  (displayln (format "Scores: A: ~a  B: ~a"
                     (view (>>> _points (_player 'A)) st)
                     (view (>>> _points (_player 'B)) st)))

  (displayln "Tokens:")
  (pretty-print (view _tokens st)))

; The End