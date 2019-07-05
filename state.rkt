#lang racket
; Set up the state and lenses
; AndrewJ 2019-07-05

; Imports
(require racket/hash
         lens/common
         ;lens/data/list
         lens/data/hash
         threading)

; Exports
(provide (all-defined-out))

; Composable versions of lens functions
(define (view _lens x) (lens-view _lens x))
(define (at _lens x) (lens-set _lens x))
(define (over _lens f x) (lens-transform _lens x f))
(define >>> lens-thrush)
(define <<< lens-compose)

;-------------------------------
; Define card types
(define card-types '(Diamond Gold Silver Cloth Spice Leather Camel))

;-------------------------------
; Define the initial game state 
(define empty-hand (hash 'Diamond 0 'Gold 0 'Silver 0
                         'Cloth 0 'Spice 0 'Leather 0
                         'Camel 0))

(define all-cards (hash 'Diamond 6 'Gold 6 'Silver 6
                        'Cloth 8 'Spice 8 'Leather 10
                        'Camel 11))

(define initial-state
  (hash  'Deck all-cards
         'Market empty-hand
         'Hand (hash 'A empty-hand
                     'B empty-hand)
         'Points (hash 'A 0
                       'B 0)
         'Tokens (hash 'Diamond '(7 7 5 5 5)
                       'Gold '(6 6 5 5 5)
                       'Silver '(5 5 5 5 5)
                       'Cloth '(5 3 3 2 2 1 1)
                       'Spice '(5 3 3 2 2 1 1)
                       'Leather '(4 3 2 1 1 1 1 1 1))))

;-------------------------------
; Lenses into the state
; Borrowing the Purescript tradition, all lenses start with an underscore.
(define _deck (hash-ref-lens 'Deck))
(define _market (hash-ref-lens 'Market))
(define _hand (hash-ref-lens 'Hand))
(define _points (hash-ref-lens 'Points))
(define _tokens (hash-ref-lens 'Tokens))
(define (_rsrc r) (hash-ref-lens r))
(define (_player p) (hash-ref-lens p))

; Composite lenses
(define (_market-rsrc r) (>>> _market (_rsrc r)))
(define (_hand-rsrc p r) (>>> _hand (_player p) (_rsrc r)))

; The End