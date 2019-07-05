#lang racket
; Model Jaipur in Racket
; AndrewJ 2019-07-05

; Imports
(require racket/hash
         lens/common
         ;lens/data/list
         lens/data/hash
         threading
         hash-ext)

; Exports
(provide (all-defined-out))

;-------------------------------
; Utility functions

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (flip f a b) (f b a))

; Composable versions of lens functions
(define (view _lens x) (lens-view _lens x))
(define (at _lens x) (lens-set _lens x))
(define (over _lens f x) (lens-transform _lens x f))
(define >>> lens-thrush)
(define <<< lens-compose)

;-------------------------------
; Define card types
(define card-types '(Diamond Gold Silver Cloth Spice Leather Camel))

(define empty-hand (hash 'Diamond 0 'Gold 0 'Silver 0
                         'Cloth 0 'Spice 0 'Leather 0
                         'Camel 0))

(define all-cards (hash 'Diamond 6 'Gold 6 'Silver 6
                        'Cloth 8 'Spice 8 'Leather 10
                        'Camel 11))

; Define the initial game state 
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

; Lenses into the state
; Borrowing the Purescript tradition, all lenses start with an underscore.
(define _deck (hash-ref-lens 'Deck))
(define _market (hash-ref-lens 'Market))
(define _hand (hash-ref-lens 'Hand))
(define _points (hash-ref-lens 'Points))
(define _tokens (hash-ref-lens 'Tokens))
(define (_rsrc r) (hash-ref-lens r))
(define (_player p) (hash-ref-lens p))

; Composite lenses
(define (_market-rsrc r) (>>> _market (_rsrc r)))
(define (_hand-rsrc p r) (>>> _hand (_player p) (_rsrc r)))

;-------------------------------
; Random card from the deck
; random-card :: State -> Card
(define (random-card st)
  (~>>
   (view _deck st)
   (hash-enumerate)
   (random-element)))

; Move n cards from _src to _dest
; If not possible then throw error
(define (move-cards rsrc _src _dest n st)
  (if (< (view (>>> _src (_rsrc rsrc)) st) n)
      (raise-user-error 'move-card "failed because ~a cards are not available to move" n)
      ;else
      (~>> st
           (over (>>> _src (_rsrc rsrc)) (curry flip - n))
           (over (>>> _dest (_rsrc rsrc)) (curry + n)))))

; Deal a card from the deck to the target
; deal-card :: State -> State
(define (deal-cards _target n st)
  (for/fold ([state st])
            ([i (range 0 n)])
    (move-cards (random-card st) _deck _target 1 state)))


;-------------------------------
; Player actions
; - Init game
; - Take cards
; - Sell cards
; - Exchange cards

; Initialise the game
; init-game :: State
(define (init-game)
  (~>>
   initial-state
   (move-cards 'Camel _deck _market 3)
   (deal-cards _market 2)
   (deal-cards (>>> _hand (_player 'A)) 5)
   (deal-cards (>>> _hand (_player 'B)) 5)))


;-------------------------------
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define jaipur-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
     (check-equal? (hash-sum (view _deck (init-game))) 40)
     ))

  (run-tests jaipur-tests))

; The End