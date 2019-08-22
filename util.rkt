#lang racket
; util.rkt
; Common utility functions
; AndrewJ 2019-08-22

; Imports
(require hash-ext)

; Exports
(provide (all-defined-out))

;-------------------------------
; Functional stuff

(define (flip f a b) (f b a))

;-------------------------------
; List functions

; Random list element
; random-element :: List a -> a
(define (random-element lst)
  (car (shuffle lst)))

; Sum a list
; list-sum :: ∀ a. [a] -> a
(define (list-sum lst) (foldl + 0 lst))

; Convert a list of numbers to an integer
; list->int :: [Int] -> Int
(define (list->int x (base 10))
  (foldl (λ (i acc)
           (+ i (* acc base)))
         0
         x))

; Return all but the last element of lst
; drop-last :: ∀ a. [a] -> [a]
(define (drop-last lst)
  (take lst (sub1 (length lst))))

; Append an item to a list in-place, and return the item
; append! :: List a -> a -> a
(define-syntax-rule (append! lst item)
  (begin
    (set! lst (append lst (list item)))
    item))

;-------------------------------
; Hash functions

; Return all the combinations of n keys from a hash with v_i copies of key k_i
; key-combinations :: Hash a b -> Integer -> List a
(define (key-combinations h n)
  (remove-duplicates (combinations (hash-enumerate h) n)))



; The End