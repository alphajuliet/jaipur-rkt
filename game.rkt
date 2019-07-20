#lang racket
; Model Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "actions.rkt"
         threading
         hash-ext
         ;plot
         )

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities

(define (flip f a b) (f b a))

; Random list element
(define (random-element lst)
  (car (shuffle lst)))

; Return all the combinations of n keys from a hash with v_i copies of key k_i
(define (key-combinations h n)
  (remove-duplicates (combinations (hash-enumerate h) n)))

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
; Choose a random action from a list.
; Pick a random top-level action first, then a random one within that list.
; choose-action :: List -> Action
(define (choose-action lst)
  (~>> lst
       (group-by car)
       (random-element)
       (random-element)))

;-------------------------------
; Apply an action to a state
; apply-action :: Action -> State -> State
(define (apply-action action state)
  (eval (append action (list state))))

;-------------------------------
; Perform a random action by a given player
; Log all actions in a global variable *game-actions*
; perform-random-action :: Player -> State -> State
(define (perform-random-action plyr st
                               #:print? [print? #f])
  (define a (available-actions plyr st))
  (define act (choose-action a))
  (if print? (displayln act) #t)
  (set! *game-actions* (append *game-actions* (list act)))
  (apply-action act st))

;-------------------------------
; Play a random game from an initial state
; Write the intermediate states to a list called *game-states*
; Put an upper bound on the moves
; random-game :: State -> State
(define (random-game initial-state
                     #:max-iterations [max-iter 100]
                     #:print? [print? #f])
  (define st initial-state)
  
  (for ([iteration (range max-iter)]
        #:break (end-of-game? st))

    ; Print current state details, if selected
    (if print?
        (begin
          (displayln (format "Iteration ~a:" iteration))
          (ppst st))
        #t)

    ; Perform a pair of moves
    (~>> st
         (perform-random-action 'A #:print? print?)
         (perform-random-action 'B #:print? print?)
         (set! st))

    ; Log the resulting state
    (set! *game-states* (append *game-states* (list st))))

  (if print?
      (begin
        (displayln "Calculate end bonus:")
        (ppst st))
      #t)
  (apply-end-bonus st))

;-------------------------------
; Plot the size of the deck over time
; g is the list of encoded states
#;(define (plot-game g)
    (define deck (map (λ (s) (apply + (take (encode-state s) 7))) g))
    (plot (lines
           (for/list ([x (range (length deck))]
                      [y (in-list deck)])
             (list x y))
           #:color 6 #:label "Size of deck")
          #:x-label "Iteration"
          #:y-label "Number of cards"))

;-------------------------------
(define s0 (init-game #:seed 1))
(define *game-states* '())
(define *game-actions* '())


;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define learn-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)

     (check-equal? (length (available-actions 'A s0)) 9)
     
     ))

  (run-tests learn-tests))


; The End