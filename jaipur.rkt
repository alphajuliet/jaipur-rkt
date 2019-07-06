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

;===============================
; Utility functions

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (flip f a b) (f b a))

(define (list-sum lst) (foldl + 0 lst))

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
      (raise-user-error 'move-card "failed because insufficient cards are available to move.")
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

; Minimum number of items that can be sold
; min-sell :: Hash Resource Int
(define min-sell
  (hash 'Diamond 2
        'Gold 2
        'Silver 2
        'Cloth 1
        'Spice 1
        'Leather 1
        'Camel 1))

; Number of cards in a player's hand, excluding camels
(define (count-cards-excl-camels plyr st)
  (define total (hash-sum (view (_hand-plyr plyr) st)))
  (define camels (view (_hand-rsrc plyr 'Camel) st))
  (- total camels))

; Take n resource tokens and add to player's score
; take-tokens :: Resource -> Player -> Int -> State -> State
(define (take-tokens rsrc plyr n st)
  (define-values (x y) (split-at (view (>>> _tokens (_rsrc rsrc)) st) n))
  (~>> st
       (over (>>> _points (_player plyr)) (curry + (list-sum x)))
       (at (>>> _tokens (_rsrc rsrc)) y)))

;===============================
; Game actions
; - Init game
; - Take cards
; - Sell cards
; - Exchange cards

;-------------------------------
; Initialise the game
; init-game :: State
(define (init-game)
  (~>> initial-state
       (move-cards 'Camel _deck _market 3)
       (deal-cards _market 2)
       (deal-cards (_hand-plyr 'A) 5)
       (deal-cards (_hand-plyr 'B) 5)))


;-------------------------------
; Take a card from the market (or all the camels)
; Deal replacement cards to the deck
; A player cannot have more than 7 cards in their hand, excluding camels
; take-card :: Player -> Resource -> State -> State
(define (take-card plyr rsrc st)

  ; If camels, then take as many as there are available
  (define n (if (eq? rsrc 'Camel)
                (view (_market-rsrc rsrc) st)
                
                ; else check that we won't blow the hand limit
                (if (>= 7 (count-cards-excl-camels plyr st))
                    (raise-user-error 'take-card "Player cannot have more than 7 cards, excluding camels.")
                    ;else
                    1)))
  (~>> st
       (move-cards rsrc _market (_hand-plyr plyr) n)
       (deal-cards _market n)))

;-------------------------------
; Sell cards
; sell-cards :: Player -> Resource -> State -> State
(define (sell-cards rsrc plyr st)
  (define n (view (_hand-rsrc plyr rsrc) st))
  (if (eq? rsrc 'Camel)
      (raise-user-error 'sell-cards "Cannot sell camels.")
      ;else
      (if (>= n (hash-ref min-sell rsrc))
          (~>> st
               (over (_hand-rsrc plyr rsrc) (curry flip - n))
               (take-tokens rsrc plyr n))
          ;else
          (raise-user-error 'sell-cards "Not enough cards to sell."))))

;-------------------------------
; Exchange cards with the market. This includes using camels. @@TODO
; exchange-cards :: Player -> Cards -> State -> State
(define (exchange-cards plyr hand st)
  st)

;===============================
; Run
(define s0 (init-game))
(ppst s0)


;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define s0 (init-game))
  (define s1 (at (_hand-rsrc 'A 'Diamond) 7 s0))
  
  (define jaipur-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)

     (test-suite
      "Init game"
      (check-equal? 40 (hash-sum (view _deck (init-game)))))

     (test-suite
      "Take card"
      (check-exn exn:fail:user?
                 (λ () (take-card 'A 'Silver s1))))
     ))

  (run-tests jaipur-tests))

; The End