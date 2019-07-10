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
(provide init-game
         take-card
         sell-cards
         exchange-cards
         end-of-game?)

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

; Take n resource tokens and add to player's score
; take-tokens :: Resource -> Player -> Int -> State -> State
(define (take-tokens rsrc plyr n st)
  
  (define-values (x y) (split-at (view (>>> _tokens (_rsrc rsrc)) st) n))
  (define (bonus-points n)
    (cond
      [(= n 3) (random-element '(1 2 3))]
      [(= n 4) (random-element '(4 5 6))]
      [(= n 5) (random-element '(8 9 10))]
      [else 0]))

  (~>> st
       (over (>>> _points (_player plyr)) (curry + (list-sum x) (bonus-points n)))
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
       (deal-cards (>>> _hand (_player 'A)) 5)
       (deal-cards (>>> _hand (_player 'B)) 5)))


;-------------------------------
; Take a card from the market (or all the camels)
; Deal replacement cards to the deck
; A player cannot have more than 7 cards in their hand, excluding camels
; take-card :: Player -> Resource -> State -> State
(define (take-card rsrc plyr st)

  (define ncamels (view (>>> _market (rsrc 'Camel)) st))
  
  (cond
    [(<= 7 (count-cards-excl-camels plyr st))
     (raise-user-error 'take-card "Player cannot have more than 7 cards, excluding camels.")]

    [(eq? rsrc 'Camel)
     (~>> st
          (move-cards rsrc _market (>>> _hand (_player plyr)) ncamels)
          (deal-cards _market ncamels))]

    [else
     (~>> st
          (move-cards rsrc _market (>>> _hand (_player plyr)) 1)
          (deal-cards _market 1))]))

;-------------------------------
; Sell cards
; sell-cards :: Player -> Resource -> State -> State
(define (sell-cards rsrc plyr st)
  
  (define n (view (>>> _hand (_player plyr) (_rsrc rsrc)) st))

  (cond
    [(eq? rsrc 'Camel)
     (raise-user-error 'sell-cards "Cannot sell camels.")]
    
    [(< n (hash-ref min-sell-hash rsrc))
     (raise-user-error 'sell-cards "Not enough resources to sell.")]
    
    [else
     (~>> st
          (over (>>> _hand (_player plyr) (_rsrc rsrc)) (curry flip - n))
          (take-tokens rsrc plyr n))]))
  
;-------------------------------
; Exchange cards with the market. This includes using camels. @@TODO
; exchange-cards :: Player -> Cards -> Cards -> State -> State
(define (exchange-cards player-cards market-cards plyr st)

  ; Helper functions
  (define (hash-min h) (apply min (hash-values h)))
  (define (enough-cards? cards _hand)
    (> 0 (hash-min (hash-sub (view _hand st) cards))))
  
  (cond
    [(not (= (hash-sum player-cards) (hash-sum market-cards)))
     (raise-user-error 'exchange-cards "Different number of resources being exchanged.")]
    
    [(or (enough-cards? player-cards (>>> _hand (_player plyr)))
         (enough-cards? market-cards _market))
     (raise-user-error 'exchange-cards "Cannot exchange resources that aren't available.")]

    [(hash-has-key? market-cards 'Camel)
     (raise-user-error 'exchange-cards "Cannot exchange a camel from the market.")]
    
    [else
     (~>> st
          ; Move player cards to market
          (over _market (curry hash-add player-cards))
          (over (>>> _hand (_player plyr)) (curry flip hash-sub player-cards))
          ; Move market cards to player
          (over (>>> _hand (_player plyr)) (curry hash-add market-cards))
          (over _market (curry flip hash-sub market-cards)))]))

;-------------------------------
; Check for end of game
; - Deck is empty
; - Three token piles are empty
; end-of-game? :: State -> Boolean
(define (end-of-game? st)

  ; Helper function
  (define token-lengths (~>> st (view _tokens) (hash-values) (map length)))
  
  (or (= 0 (hash-sum (view _deck st)))
      (= 3 (length (filter (curry = 0) token-lengths)))))


;===============================
; Run



;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define s0 (init-game))
  (define s1 (at (>>> _hand (_player 'A) (_rsrc 'Diamond)) 7 s0))
  
  (define jaipur-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)

     (test-suite
      "Init game"
      (check-equal? 40 (hash-sum (view _deck (init-game)))))

     #;(test-suite
        "Take card"
        (check-exn exn:fail:user?
                   (λ () (take-card 'A 'Diamond s1))))
     ))

  (run-tests jaipur-tests))

; The End