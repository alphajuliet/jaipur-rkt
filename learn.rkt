#lang racket
; Q-learning implementation for Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "actions.rkt"
         "game.rkt"
         threading
         math/array
         )

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities

; Convert a list of numbers to an integer
; list->int :: List Int -> Int
(define (list->int x)
  (foldl (λ (i acc) (+ i (* acc 10))) 0 x))

; Return all but the last element of lst
; drop-last :: List a -> List a
(define (drop-last lst)
  (take lst (sub1 (length lst))))

; Filter actions on a given player
; Usage: (filter (action-player 'A) actions-list)
; action-player :: Player -> Action -> Boolean
(define (action-player plyr)
  (λ (x) (eq? (eval (last x)) plyr)))

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


; Encode a state
; This is from the point of view of player A. All they can see is the
; market (m), their own hand (h), and the token stacks (t).
; Assume that points are unimportant.
; encode-state :: State -> List Integer
(define (encode-state st)
  (define m (view _market st))
  (define h (view (>>> _hand (_player 'A)) st))
  (define t (view _tokens st))
  
  (list (hash-values m)
        (hash-values h)
        (map length (hash-values t))
        #;(map (compose sgn length)
             (hash-values t))))

; Encode an action
; We encode an action (without state) as an integer.
; Currently encoded as 1-3.
; encode-action :: Action -> Integer
(define (encode-action act)
  (cond [(eq? (car act) 'take-card) 1]
        [(eq? (car act) 'sell-cards) 2]
        [else 3]))


; Run the Q-learning cycle
(define (q-learn)
  (define s (init-game))
  #f)


;-------------------------------
; Explore

(define (show-actions)
  (~>> *game*
       (list-actions)
       (filter (action-player 'A))
       (map encode-action)))

(define (show-states)
  (~>> *game*
       (list-states)
       (map encode-state)))

; The End