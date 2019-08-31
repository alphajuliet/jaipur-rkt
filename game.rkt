#lang racket
; Model Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "actions.rkt"
         "util.rkt"
         threading
         hash-ext
         racket/trace
         ;plot
         )

; Exports
(provide available-actions
         apply-action
         apply-policy
         play-game
         play-game-result
         policy-random
         write-game
         
         s0
         *game*
         list-states
         list-actions
         iterations)

;-------------------------------
; Utilities

; Execute f with arg if flag is true, and return the arg
; This provides an optional side-effect in the middle of a ~>> threading chain.
(define (do-if flag f arg . rest)
  (begin
    (cond [flag (apply f (cons arg rest))])
    (last (cons arg rest))))

#;(define-syntax (do-if* test? proc arg)
  `((λ (x) (cond [,test? ,proc]) x)))

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
    (for*/list
        ([n (range 2 (add1 (count-cards-excl-camels market-cards)))]
         [x (cartesian-product (key-combinations player-cards n)
                               (key-combinations (hash-remove market-cards 'Camel) n))]
         ; Check that we're not ending up with more than 7 hand cards by swapping camels.
         ; Error if (camels being swapped + number of non-camel hand cards) > 7.
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
(define (pick-random-action lst)
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
; Apply a given policy function to generate the next state

; type Policy = Player -> State -> Action
; apply-policy :: Policy -> Player -> State -> State
(define (apply-policy policy plyr st)  
  (~>> st
       (policy plyr) ; Action
       (append! *game*)
       (apply-action _ st)
       (append! *game*)))

;-------------------------------
; Play a game, using a given policy function
; play-game :: Policy -> State -> State
(define (play-game policy initial-state
                   #:max-iterations [max-iter 100])

  ; Iterate through the actions to generate a final state
  (set! *game* '())
  (append! *game* initial-state)
  (~>> (for/fold ([st initial-state])
                 ([iteration (range max-iter)]
                  #:break (> iteration max-iter)
                  #:break (end-of-game? st))
         (~>> st
              (begin (append! *game* (format "## Iteration: ~a" iteration)))
              (apply-policy policy 'A)
              (apply-policy policy 'B)))

       ; Add the bonus points for the more camels
       (apply-end-bonus)))

; Play a game and only return the final scores
(define (play-game-result policy initial-state)
  (~>> initial-state
       (play-game policy)
       (view _points)
       (hash-values)))

;-------------------------------
; Play a completely random game

; policy-random :: Policy
(define (policy-random player state)
  (pick-random-action (available-actions player state)))

; random-game :: State -> State
(define (random-game initial-state)
  (play-game policy-random initial-state))


;-------------------------------
; Write the *game* list to a file
(define (write-game fname
                    #:action (action-fn identity)
                    #:state (state-fn identity))
  
  (call-with-output-file fname #:exists 'replace
    (λ (out)
      (for ([e (in-list *game*)])
        (let ([p (cond [(hash? e) (state-fn e)]
                       [else (action-fn e)])])
          (println p out))))))

;-------------------------------
(define s0 (init-game #:seed 1))

; Record the game
(define *game* '())
(define *print*? #f)
(define (list-states g) (filter hash? g))
(define (list-actions g) (filter list? g))
(define (iterations g) (last (filter string? g)))


;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define game-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
     (check-equal? (length (available-actions 'A s0)) 9)))

  (run-tests game-tests))

; The End