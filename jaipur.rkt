#lang racket
; Model Jaipur in Racket
; AndrewJ 2019-07-05

; Imports
(require racket/hash
         lens/common
         lens/data/hash
         threading
         hash-ext
         "state.rkt")

; Exports
(provide (all-defined-out))

;-------------------------------
; Utility functions

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (flip f a b) (f b a))

;-------------------------------
; Random card from the deck
; random-card :: State -> Card
(define (random-card st)
  (~>> (view _deck st)
       (hash-enumerate)
       (random-element)))

; Move n cards from _src to _dest
; If not possible then throw an error
(define (move-cards rsrc _src _dest n st)
  (if (> n (view (>>> _src (_rsrc rsrc)) st))
      (raise-user-error 'move-card "failed because insufficient cards are available to move." n)
      ;else
      (~>> st
           (over (>>> _src (_rsrc rsrc)) (curry flip - n))
           (over (>>> _dest (_rsrc rsrc)) (curry + n)))))

; Deal a card from the deck to the target
; deal-card :: State -> State
(define (deal-cards _target n st)
  (for/fold ([state st])
            ([i (range n)])
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
  (~>> initial-state
       (move-cards 'Camel _deck _market 3)
       (deal-cards _market 2)
       (deal-cards (>>> _hand (_player 'A)) 5)
       (deal-cards (>>> _hand (_player 'B)) 5)))


; Take a card from the market (or all the camels)
; Deal replacement cards to the deck
; take-card :: Player -> Card -> State -> State
(define (take-card plyr rsrc st)
  (define n (if (eq? rsrc 'Camel)
                (view (>>> _market (_rsrc rsrc)) st)
                1))
  (define _dest (>>> _hand (_player plyr)))
  (~>> st
       (move-cards rsrc _market _dest n)
       (deal-cards _market n)))

;-------------------------------
(define s0 (init-game))

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