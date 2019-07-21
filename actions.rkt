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
         end-of-game?
         apply-end-bonus
         
         ; Action tests
         take-card-invalid?
         sell-cards-invalid?
         exchange-cards-invalid?)

;===============================
; Utility functions

(define (random-element lst)
  (car (shuffle lst)))

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
      (raise-user-error 'move-card
                        (format "failed because insufficient ~a cards are available to move from ~a."
                                rsrc (view _src st)))
      ;else
      (~>> st
           (over (>>> _src (_rsrc rsrc)) (curry flip - n))
           (over (>>> _dest (_rsrc rsrc)) (curry + n)))))

; Deal a card from the deck to the target
; deal-card :: State -> State
(define (deal-cards _target n state)
  (define st state)
  (define n1 (min n (hash-sum (view _deck state)))) ;only deal as many as are left
  (for ([i (range n1)])
    (set! st (move-cards (random-card st) _deck _target 1 st)))
  st)

; Take n resource tokens and add to player's score
; take-tokens :: Resource -> Player -> Int -> State -> State
(define (take-tokens rsrc plyr n st)
  
  (define t (view (>>> _tokens (_rsrc rsrc)) st))
  
  (define-values (x y)
    (if (>= n (length t))
        (values t '())
        ;else
        (split-at t n)))
  
  (define (bonus-points n)
    (cond
      [(= n 3) (random-element '(1 2 3))]
      [(= n 4) (random-element '(4 5 6))]
      [(= n 5) (random-element '(8 9 10))]
      [else 0]))

  (~>> st
       (over (>>> _points (_player plyr))
             (curry + (list-sum x) (bonus-points n)))
       (at (>>> _tokens (_rsrc rsrc)) y)))


;===============================
; Game actions
; - Init game
; - Take cards
; - Sell cards
; - Exchange cards

;-------------------------------
; Initialise the game, with an optional seed > 0
; init-game :: Int? -> State
(define (init-game #:seed [seed 0])
  (if (> seed 0) (random-seed seed) #t)
  (~>> initial-state
       (move-cards 'Camel _deck _market 3)
       (deal-cards _market 2)
       (deal-cards (>>> _hand (_player 'A)) 5)
       (deal-cards (>>> _hand (_player 'B)) 5)))

;-------------------------------
; Take a card from the market (or all the camels)
; Deal replacement cards to the deck

; Invalid if:
; - Player is not taking camels, and already has 7 non-camel cards in their hand

; take-card-invalid? :: Resource -> Player -> State -> Boolean | String
(define (take-card-invalid? rsrc plyr st)
  (define player-hand (view (>>> _hand (_player plyr)) st))
  (cond [(and (not (eq? rsrc 'Camel))
              (>= (count-cards-excl-camels player-hand) 7))
         (format "Player ~a cannot have more than 7 cards, excluding camels." plyr)]
        [else #f]))

; take-card :: Resource -> Player -> State -> State
(define (take-card rsrc plyr st)
  
  (define n-market-camels (view (>>> _market (_rsrc 'Camel)) st))
  (define player-hand (view (>>> _hand (_player plyr)) st))
  (define error? (take-card-invalid? rsrc plyr st))
  
  (cond [(string? error?)
         (raise-user-error 'take-card error?)]

        [(eq? rsrc 'Camel)
         (~>> st
              (move-cards rsrc _market (>>> _hand (_player plyr)) n-market-camels)
              (deal-cards _market n-market-camels))]

        [else
         (~>> st
              (move-cards rsrc _market (>>> _hand (_player plyr)) 1)
              (deal-cards _market 1))]))

;-------------------------------
; Sell cards

; sell-cards-invalid? :: Player -> Resource -> State -> Boolean | String|
(define (sell-cards-invalid? rsrc plyr st)
  (define n (view (>>> _hand (_player plyr) (_rsrc rsrc)) st))
  (cond
    [(eq? rsrc 'Camel)
     (format "Player ~a cannot sell camels." plyr)]
    [(< n (hash-ref min-sell-hash rsrc))
     (format "Player ~a does not enough ~a cards to sell." plyr rsrc)]
    [else #f]))

; sell-cards :: Player -> Resource -> State -> State
(define (sell-cards rsrc plyr st)
  
  (define n (view (>>> _hand (_player plyr) (_rsrc rsrc)) st))
  (define error? (sell-cards-invalid? rsrc plyr st))

  (cond [(string? error?)
         (raise-user-error 'sell-cards error?)]
        [else
         (~>> st
              (over (>>> _hand (_player plyr) (_rsrc rsrc)) (curry flip - n))
              (take-tokens rsrc plyr n))]))
  
;-------------------------------
; Exchange cards with the market. This includes using camels. @@TODO

(define (exchange-cards-invalid? player-cards market-cards plyr st)
  (define (hash-min h) (apply min (hash-values h)))
  (define (enough-cards? cards _hand)
    (> 0 (hash-min (hash-sub (view _hand st) cards))))
  (define player-hand (view (>>> _hand (_player plyr)) st))

  (cond
    [(not (= (hash-sum player-cards) (hash-sum market-cards)))
     "Different number of resources being exchanged."]
    [(or (enough-cards? player-cards (>>> _hand (_player plyr)))
         (enough-cards? market-cards _market))
     "Cannot exchange resources that aren't available."]
    [(hash-has-key? market-cards 'Camel)
     "Cannot exchange a camel from the market."]
    [(> (+ (count-cards-excl-camels player-hand) (hash-ref player-cards 'Camel 0)) 7) 
     "Cannot have more than 7 hand cards after exchange."]
    [else #f]))

; exchange-cards :: Player -> Cards -> Cards -> State -> State
(define (exchange-cards player-cards market-cards plyr st)

  ; Helper functions
  (define (hash-min h) (apply min (hash-values h)))
  (define (enough-cards? cards _hand)
    (> 0 (hash-min (hash-sub (view _hand st) cards))))
  (define player-hand (view (>>> _hand (_player plyr)) st))
  (define error? (exchange-cards-invalid? player-cards market-cards plyr st))
  
  (cond
    [(string? error?)
     (raise-user-error 'exchange-cards error?)]    
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

;-------------------------------
; Add end-of-game bonus
(define (apply-end-bonus st)
  (if (> (view (>>> _hand (_player 'A) (_rsrc 'Camel)) st)
         (view (>>> _hand (_player 'B) (_rsrc 'Camel)) st))
      (over (>>> _points (_player 'A)) (curry + 5) st)
      (over (>>> _points (_player 'B)) (curry + 5) st)))

;===============================
; Run



;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define s0 (init-game #:seed 1))
  
  (define jaipur-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
     (check-equal? 40 (hash-sum (view _deck (init-game))))

     ; Take card
     (let ([s1 (at (>>> _hand (_player 'A) (_rsrc 'Spice)) 7 s0)])
       (check-true (string? (take-card-invalid? 'Spice 'A s1))))

     ; Sell cards
     (check-true (string? (sell-cards-invalid? 'Diamond 'B s0)))
     (let* ([s1 (at (>>> _tokens (_rsrc 'Spice)) '(1) s0)]
            [s2 (sell-cards 'Spice 'A s1)])
       (check-equal? (view (>>> _tokens (_rsrc 'Spice)) s2) '()))

     ; Exchange cards
     (let ([p (hash 'Silver 1 'Leather 1)]
           [m (hash 'Gold 1 'Spice 1)])
       (check-true (string? (exchange-cards-invalid? p m 'A s0))))
     
     ))

  (run-tests jaipur-tests))

; The End