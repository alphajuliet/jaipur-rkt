#lang racket
; Q-learning implementation for Jaipur
; AndrewJ 2019-07-09

; Imports
(require "util.rkt"
         "state.rkt"
         "actions.rkt"
         "game.rkt"
         threading
         hash-ext
         data/monad
         data/maybe)

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities

(define >>= chain)

; Filter actions on a given player
; Usage: (filter (action-player 'A) actions-list)
; action-player :: Player -> Action -> Boolean
(define (action-player plyr)
  (λ (x) (eq? (eval (last x)) plyr)))

; Return a sorted list of keys
; Is this necessary?
(define (sort-keys h)
  (map string->symbol
       (~>> h
            (hash-keys)
            (map symbol->string)
            (sort _ string<=?))))

;-------------------------------
; Encodings

; Encode a state
; This is from the point of view of player A. All they can see is the
; market (m), their own hand (h), and the token stacks (t).
; Return the current score for player A too.

; encode-state :: State -> Integer
(define (encode-state st)
  (define m (~>> st
                 (view _market)
                 (hash-values)))
  (define h (~>> st
                 (view (>>> _hand (_player 'A)))
                 (hash-values)))
  (define t (~>> st
                 (view _tokens)
                 (hash-values)
                 (map length)))

  ; Convert the lists into a single number via binary strings
  (~>> (list m h t)
       (flatten)
       (map sgn)
       (flip list->int 2)))

; Encode an action
; We encode an action (without state).
; Currently encoded as a list. The first element is a code 0-2 for the action
; followed by either 1 or 2 one-hot-encoded parameters for the resource.
; e.g. (encode-action '(take-card 'Camel 'A)) => '(0 64)

; encode-action :: Action -> [Integer]
(define (encode-action act)

  ; Convert a hash to a boolean 1-hot encoded decimal number
  ; (1-hot '#hash('Camel 1 'Silver 5)) => 36
  (define (1-hot h)
    (~>> h
         (hash-add _ empty-hand)
         (hash-values)
         (map sgn)
         (flip list->int 2)))

  (cond [ (eq? (car act) 'take-card)
          (list 0 (1-hot (hash (eval (second act)) 1))) ]
        [ (eq? (car act) 'sell-cards)
          (list 1 (1-hot (hash (eval (second act)) 1))) ]
        [ else
          (list 2 (1-hot (second act)) (1-hot (third act))) ]))


;-------------------------------
; Q-learning algorithm
;
; Algorithm parameters: step size α ∈ (0, 1], small ε > 0
; Initialise Q(s, a), for all s ∈ S+, a ∈ A(s), arbitrarily except that Q(terminal, ·) = 0
;
; Loop for each episode:
;	Initialise S
;	Loop for each step of episode:
;		Choose A from S using policy derived from Q (e.g., ε-greedy) 
;		Take action A, observe R, S'
;		Q(S,A) ← Q(S,A) + α[R + γ max_a Q(S',a) – Q(S,A)]
;		S ← S'
;	until S is terminal

; Define the mapping Q: State x Action -> Real
; This is too large and sparse for an array, so we use a (mutable) hash of (mutable) hashes.
(define Q (make-hash))

; Q-set! :: ∀ a b. a -> b -> Real -> Real
(define (Q-set! state action value)
  (if (hash-has-key? Q state)
      (hash-set! (hash-ref Q state) action value)
      ; else
      (hash-set! Q state (make-hash `((,action . ,value)))))
  value)

; Do a safe lookup
; Q-ref :: ∀ a b. a -> b -> Real 
(define (Q-ref state action)
  (~>> (>>= (λ (m) (hash-ref+ m action))
            (hash-ref+ Q state))
       (from-just '(0.))))


(define gamma 0.99) ; discounting factor
(define alpha 0.5)  ; soft update parameter

; Return the action with the most points for a given player
; argmax-points :: Policy
(define (argmax-points player state)
  
  (define (action->points act st)
    (view (>>> _points (_player player))
          (apply-action act state)))
  
  (argmax (λ (act) (action->points act state))
          (available-actions player state)))

; Run the Q-learning cycle
(define (update-Q curr-state reward action next-state done?)
  (define next-actions (available-actions next-state))
  (define next-states (map (λ (a) (apply-action a curr-state))
                           next-actions))
  (define max-q-next
    (apply max (Q-ref (encode-state next-states)
                      (encode-action action))))
  #f)

;-------------------------------
; Explore

; Define a policy where player A always picks the most points, and player B picks randomly
(define (policy-semi player state)
  (cond [(eq? player 'A)
         (argmax-points 'A state)]
        [else
         (policy-random 'B state)]))

;-------------------------------
; Run n games with a given policy and write them to game files

(define (n-games policy (n 2))
  (for ([i (in-range 0 n)])
    (play-game policy (init-game #:seed i))
    (write-game (format "games/game-~a.txt" i))))

(define (scoreA st)
  (view (>>> _points (_player 'A)) st))

(define (show-actions)
  (~>> *game*
       (list-actions)
       (filter (action-player 'A))
       (map encode-action)))

(define (show-states)
  (~>> *game*
       (list-states)
       (map encode-state)))

;===============================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define learn-tests
    (test-suite
     "Unit tests"
     (check-equal? (encode-state s0) 365435)
     
     (let ([x (Q-set! 1234 5678 1.0)]
           [y (Q-set! 1234 5679 2.0)])
       (check-equal? (Q-ref 1234 5678) 1.0)
       (check-equal? (Q-ref 1234 5679) 2.0))))

  (run-tests learn-tests))



; The End