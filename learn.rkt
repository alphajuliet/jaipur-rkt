#lang racket
; Q-learning implementation for Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "jaipur.rkt"
         hash-ext)

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities

; Random list element
(define (random-element lst)
  (list-ref lst (random (length lst))))

; Return all the combinations of n keys from a hash with v_i copies of key k_i
(define (key-combinations h n)
  (remove-duplicates (combinations (hash-enumerate h) n)))

; Apply an action to a state
(define (apply-action action state)
  (eval (append action (list state))))

;-------------------------------
; List available actions, given the current state, and whose turn it is
; available-actions :: State -> Player -> [Action]
(define (available-actions plyr st)
  (define actions '())

  ; Facts
  (define market-camels (view (>>> _market (_rsrc 'Camel)) st))
  (define market-cards (view _market st))
  (define player-cards (view (>>> _hand (_player plyr)) st))
  (define n-player-cards (count-cards-excl-camels plyr st))
  (define available-market-cards
    (for/list ([(k v) (in-hash (view _market s0))]
               #:when (> v 0))
      k))

  ; ----------------
  ; Take card:
  ; 1. Camels if any are in the market
  ; 2. Any (non-camel) resource from the market if player hand contains less than 7 cards

  (define (take-card-options)
    (append
     ; case 1
     (if (> market-camels 0)
         (list `(take-card 'Camel ',plyr))
         '())
     ; case 2
     (for/list ([(k v) (in-hash market-cards)]
                #:unless (eq? k 'Camel)
                #:when (> v 0)
                #:when (< n-player-cards 7))
       `(take-card ',k ',plyr))))
    
  ; ----------------
  ; Sell cards:
  ; - Any hand cards with the minimum sell quantity, apart from camels
  
  (define (sell-cards-options)
    (for/list ([(k v) (in-hash player-cards)]
               #:when (>= v (min-sell k))
               #:unless (eq? k 'Camel))
      `(sell-cards ',k ',plyr)))

  ; ----------------
  ; Exchange cards:
  ; - Min of 2 cards
  ; - Does not include market camels
  ; - Source and target cards must be different

  (define (exchange-cards-options)
    (for/list
        ([x (cartesian-product (key-combinations player-cards 2)
                               (key-combinations (hash-remove market-cards 'Camel) 2))]) 
      `(exchange-cards ,(hash-collect (car x)) ,(hash-collect (cadr x)) ',plyr)))

  (append (take-card-options)
          (sell-cards-options)
          (exchange-cards-options)))

(define s0 (init-game))


; The End