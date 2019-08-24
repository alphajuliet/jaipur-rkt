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
(provide (all-defined-out))

;-------------------------------
; Utilities

; Execute f with arg if flag is true, and return the arg
; This provides an optional side-effect in the middle of a ~>> threading chain.
(define (do-if flag f arg . rest)
  (begin
    (cond [flag (apply f (cons arg rest))])
    (last (cons arg rest))))

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
(define (choose-action lst)
  (~>> lst
       (group-by car)
       (random-element)
       (random-element)))

;-------------------------------
; Apply an action to a state
; apply-action :: Action -> State -> State
(define (apply-action action state)
  (define st (eval (append action (list state))))
  (append! *game* action)
  (append! *game* st)
  st)

;-------------------------------
; Perform a random action by a given player
; Log all actions in a global variable *game-actions*
; perform-random-action :: Player -> State -> State
(define (perform-random-action plyr st
                               #:print? [print? #f])
  
  (define (print-action-if flag action)
    (cond [flag (displayln action)])
    action)

  (~>> st
       (available-actions plyr)
       (choose-action)
       (print-action-if print?)
       (flip apply-action st)))

;-------------------------------
; Play a random game from an initial state
; Write the intermediate states to a list called *game-states*
; Put an upper bound on the moves
; random-game :: State -> State
(define (random-game initial-state
                     #:max-iterations [max-iter 100]
                     #:print? [print? #f])

  ; Printing functions
  (define (print-header-if flag i st)
    (cond [flag (displayln (format "\n# Iteration ~a:" i))])
    st)

  (define (print-state-if flag st)
    (cond [flag (ppst st)])
    st)
  
  ; Iterate through the actions to generate a final state
  (set! *game* '())
  (cond [print? (displayln "# New game\n")])
  (print-state-if print? initial-state)
  (~>> (for/fold ([st initial-state])
                 ([iteration (range max-iter)]
                  #:break (end-of-game? st))
         (~>> st
              (print-header-if print? iteration)
              (perform-random-action 'A #:print? print?)
              (perform-random-action 'B #:print? print?)
              (print-state-if print?)))

       ; Add the bonus points for the more camels
       (print-header-if print? "Bonus")
       (apply-end-bonus)
       (print-state-if print?)))

;-------------------------------
; Plot the size of the deck over time
; g is the list of encoded states
#;(define (plot-game g)
    (define deck
      (map (λ (s) (apply + (take (encode-state s) 7))) g))
    (plot
     (lines (for/list ([x (range (length deck))]
                       [y (in-list deck)])
              (list x y))
            #:color 6
            #:label "Size of deck")
     #:x-label "Iteration"
     #:y-label "Number of cards"))

;-------------------------------
; Write a random game to a text file
; write-random-game :: String -> State -> State
(define (write-random-game fname s0)
  (define out
    (open-output-file fname #:exists 'append))
  (parameterize ([current-output-port out])
    (random-game s0 #:print? #t))
  (close-output-port out))


;-------------------------------
(define s0 (init-game #:seed 1))

; Record the game
(define *game* '())
(define (list-states g) (filter hash? g))
(define (list-actions g) (filter (compose not hash?) g))

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