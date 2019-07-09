#lang racket
; Q-learning implementation for Jaipur
; AndrewJ 2019-07-09

; Imports
(require "state.rkt"
         "jaipur.rkt")

; Exports
(provide (all-defined-out))

;-------------------------------
; Utilities
(define-syntax-rule (append! lst elt)
  (set! lst (append lst elt)))

;-------------------------------
; List available actions, given the current state, and whose turn it is
; available-actions :: State -> Player -> [Action]
(define (available-actions plyr st)
  (define actions '())

  ; Facts
  (define market-camels (view (>>> _market (_rsrc 'Camel)) st))
  (define player-cards (count-cards-excl-camels plyr st))
  (define available-market-cards
    (for/list ([(k v) (in-hash (view _market s0))]
               #:when (> v 0))
      k))

  ; Take cards
  (append! actions
           (if (< player-cards 7)
               (for/list ([(k v) (in-hash (view _market s0))]
                          #:when (> v 0))
                 `(take-card ,k ,plyr))
               '()))
  
  ; Exchange cards
  
  ; Sell cards

  actions
  )

(define s0 (init-game))


; The End