#lang racket
; Q-learning implementation for Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "jaipur.rkt"
         threading
         hash-ext)

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities

; Random list element
(define (random-element lst)
  (car (shuffle lst)))

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
  (define n-player-cards (count-cards-excl-camels player-cards))
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
               #:unless (sell-cards-invalid? k plyr st))
      `(sell-cards ',k ',plyr)))

  ; ----------------
  ; Exchange cards:
  ; - Min of 2 cards
  ; - Does not include market camels
  ; - Source and target cards must be different

  (define (exchange-cards-options)
    (for/list
        ([x (cartesian-product (key-combinations player-cards 2)
                               (key-combinations (hash-remove market-cards 'Camel) 2))]
         ; Check that we're not ending up with more than 7 hand cards by swapping camels
         ; error if camels being swapped + number of non-camel hand cards > 7
         #:unless (> (+ (count (curry eq? 'Camel) (car x))
                        (count-cards-excl-camels player-cards))
                     7))
      `(exchange-cards ,(hash-collect (car x)) ,(hash-collect (cadr x)) ',plyr)))

  (append (take-card-options)
          (sell-cards-options)
          (exchange-cards-options)))

;-------------------------------
; Q-learning algorithm
;
; Algorithm parameters: step size α ε (0, 1], small ε > 0
; Initialise Q(s, a), for all s ∈ S+,a ∈ A(s), arbitrarily except that Q(terminal, ·) = 0
;
; Loop for each episode:
;	Initialise S
;	Loop for each step of episode:
;		Choose A from S using policy derived from Q (e.g., ε-greedy) 
;		Take action A, observe R, S'
;		Q(S,A) ← Q(S,A) + α[R + γ max_a Q(S',a) – Q(S,A)]
;		S ← S'
;	until S is terminal

; Encode a state in the Q-table
(define (encode-state st)
  (flatten
   (append (hash-values (view _deck st))
           (hash-values (view _market st))
           (hash-values (view (>>> _hand (_player 'A)) st))
           (hash-values (view (>>> _hand (_player 'B)) st)))))

; Encode an action in the Q-table
(define (encode-action act)
  (take act (sub1 (length act))))

; Run the Q-learning cycle
(define (q-learn)
  (define s (init-game))
  #f)

; Perform a random action by a given player
(define (perform-random-action plyr st
                               #:print? [print? #f])
  (define act
    (random-element (available-actions plyr st)))
  (if print? (displayln act) #t)
  (apply-action act st))

; Play a random game from an initial state
; Write the intermediate states to a list called *game-states*
; Put an upper bound on the moves
(define (random-game init-state
                     #:max-iterations [max-iter 100]
                     #:print? [print? #f])
  (define st init-state)
  
  (for ([iteration (range max-iter)]
        #:break (end-of-game? st))
    (if print?
        (begin
          (displayln (format "Iteration ~a:" iteration))
          (ppst st))
        #t)
    (~>> st
         (perform-random-action 'A #:print? print?)
         (perform-random-action 'B #:print? print?)
         (set! st))
    (set! *game-states* (append *game-states* (list st))))
  st)

;-------------------------------
(define s0 (init-game #:seed 42))
(define *game-states* '())


;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define learn-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)

     (check-equal? (length (available-actions 'A s0)) 12)
     
     ))

  (run-tests learn-tests))


; The End