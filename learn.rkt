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

; Apply an action to a state
(define (apply-action action state)
  (eval (append action (list state))))


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


; Encode a state
; This is from the point of view of player A. All they can see is the
; market, their hand, and the token stacks.
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
       (flip modulo 65521)))

; Encode an action
(define (encode-action act)
  (take act (sub1 (length act))))

; Run the Q-learning cycle
(define (q-learn)
  (define s (init-game))
  #f)


; The End