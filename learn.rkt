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

; Return all but the last element of lst
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
; market, their own hand, and the token stacks. Assume that points are unimportant.
; We encode it in ~16 bits
; encode-state :: State -> Integer
(define (encode-state st)
  (define (list->int x)
    (foldl (λ (i acc) (+ i (* acc 10))) 0 x))

  (~>> (append (hash-values (view _market st))
               (hash-values (view (>>> _hand (_player 'A)) st))
               (map length (hash-values (view _tokens st))))
       (flatten)
       (list->int)
       (flip modulo 65521))) ; log_2(65521) = 15.9997

; Encode an action
; We encode an action (without state) as an integer
; encode-action :: Action -> Integer
(define (encode-action act)
  (drop-last act))

; Run the Q-learning cycle
(define (q-learn)
  (define s (init-game))
  #f)

;-------------------------------
; Explore

(define (show-actions)
  (~>> *game-actions*
       (filter (action-player 'A))
       (map encode-action)))

; The End