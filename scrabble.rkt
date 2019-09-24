#lang racket
(require rackunit)
(provide subbag?)


#|
KEY

Helper Functions:
- subbag?
- best-word-helper
- assoc
- score-letter
- score-word
- score-word-helper
- subbag-of-rack
- scrabble-tile-bag

Function:
- best-word
|#

#|
Input: Two lists, S and B.
Output: True if list B contains all the elements in S in quanitities at least as large.
False if this is not True.
|#

(define (subbag? S B)
  (cond [(empty? S) #t]
        [(member (first S) B)
         (subbag? (rest S) (remove (first S) B))]
        [else #f]))
  
; Student Tests
(check-equal? (subbag? '(s)   '(s p a m s))   true)
(check-equal? (subbag? '(s)   '())   false)
(check-equal? (subbag? '()   '())   true)
(check-equal? (subbag? '(s s s)   '(s p a m s))   false)

; Provided Tests
(check-equal? (subbag? '()      '(s p a m s)) true)
(check-equal? (subbag? '(s s)   '(s p a m s))   true)
(check-equal? (subbag? '(s m)   '(s p a m s))   true)
(check-equal? (subbag? '(a p)   '(s p a m s))   true)
(check-equal? (subbag? '(a m a) '(s p a m s))   false)
(check-equal? (subbag? '(a s)   '(s a))         true)


;; scrabble-tile-bag  
;;   letter tile scores and counts from the game of Scrabble
;;   the counts aren't needed they're obtained from
;;   http://en.wikipedia.org/wiki/Image:Scrabble_tiles_en.jpg
;;

#|
Output: Score of scrabble tiles! Yay!
|#

(define scrabble-tile-bag
  '((#\a 1 9) (#\b 3 2) (#\c 3 2) (#\d 2 4) (#\e 1 12)
   (#\f 4 2) (#\g 2 3) (#\h 4 2) (#\i 1 9) (#\j 8 1)
   (#\k 5 1) (#\l 1 4) (#\m 3 2) (#\n 1 6) (#\o 1 8)
   (#\p 3 2) (#\q 10 1)(#\r 1 6) (#\s 1 4) (#\t 1 6)
   (#\u 1 4) (#\v 4 2) (#\w 4 2) (#\x 8 1) (#\y 4 2)
   (#\z 10 1) (#\_ 0 2)) ) 
;; end define scrabble-tile-bag
;;
;; The underscore will be used to represent a blank tile, which is a wild-card


#|
Input: A word list, WL (pairs of words and scores).
Output: The word and best score.
|#

(define (best-word-helper WL)
  (cond [(equal? (length WL) 0) '("" 0)]
        [(equal? (length WL) 1) (first WL)]
        [(> (second (first WL)) (second (second WL)))
         (best-word-helper (cons (first WL) (rest (rest WL))))]
        [else (best-word-helper (rest WL))])
  )

; Student Tests
(check-equal? (best-word-helper '()) '("" 0))
(check-equal? (best-word-helper '(("cay" 8))) '("cay" 8))
(check-equal? (best-word-helper '(("cay" 8) ("a" 1))) '("cay" 8))


#|
Input: List of list and element e.
Output: The LoL where element e occurs.
|#

(define (assoc e A)
  (cond [(empty? A) #f]
        [(equal? e (first (first A))) (first A)]
        [(assoc e (rest A))]
        [else #f])
  )

; Student Tests
(check-equal? (assoc 0 '()) #f)
(check-equal? (assoc 0 '((0 a) (1 b) (2 c) (3 d))) '(0 a))
(check-equal? (assoc 4 '((1 a) (2 b) (3 c) (4 d))) '(4 d))
(check-equal? (assoc 5 '((0 a) (1 b) (2 c) (3 d))) #f)

#|
Input: Letter l.
Output: The score of letter l.
|#

(define (score-letter l)
  (second (assoc l scrabble-tile-bag))
  )

; Student Test
(check-equal? (score-letter '#\w) 4)
(check-equal? (score-letter '#\a) 1)

#|
Input: Word w.
Output: Score of word, w as an integer.
|#

(define (score-word w)
  (cond [(equal? w "") 0]
    [else (+ (score-letter (first (string->list w)))
             (score-word (list->string (rest (string->list w)))))]
    )
  )

; Student Test
(check-equal? (string->list "") '())
(check-equal? (string->list "hi") '(#\h #\i))
(check-equal? (list->string '(#\h #\i)) "hi")


#|
Input: The result of function score-word.
Output: A list of w and score-word.
|#
(define (score-word-helper w)
  (cons w (list(score-word w)))
)
; Student Test
(check-equal? (score-word-helper "zzz") '("zzz" 30))
(check-equal? (score-word-helper "") '("" 0))

#|
Input: Inputs rack and WL.
Output: All of the rack and WL that pass subbag or the elements
that can be created based on the rack.
|#
(define (subbag-of-rack rack WL)
  (cond [(empty? WL) '()]
        [(equal? (subbag? (string->list (first WL)) (string->list rack)) #t)
         (cons (list (first WL) (score-word (first WL))) (subbag-of-rack rack (rest WL)))]
        [else (subbag-of-rack rack (rest WL))])
)

; Student Test
(check-equal? (subbag-of-rack "kwyjibo" '()) '())
(check-equal? (subbag-of-rack "kwyjibo" '("ace" "ade" "cad" "cay" "day")) '())
(check-equal? (subbag-of-rack "wa" '("xz" "wa" "a" "w" "trt")) '(("wa" 5) ("a" 1) ("w" 4)))


#|
Input: Letters on scrabble rack, rack, and list of words that might be
made from rack, WL.
Output: The highest scoring word in list WL that uses letters from rack!
|#

(define (best-word rack WL)
  (best-word-helper (subbag-of-rack rack WL)))

; Student Tests
(check-equal? (best-word "" '())  '("" 0))
(check-equal? (best-word "academy" '())  '("" 0))
(check-equal? (best-word "aaa" '("a"))  '("a" 1))
(check-equal? (best-word "ababa" '("a"))  '("a" 1))

; Provided Tests
(check-equal? (best-word "academy" '("ace" "ade" "cad" "cay" "day"))  '("cay" 8))
(check-equal? (best-word "appler" (list "peal" "peel" "ape" "paper")) '("paper" 9))
(check-equal? (best-word "paler" (list "peal" "peel" "ape" "paper"))  '("peal" 6))
(check-equal? (best-word "kwyjibo" '("ace" "ade" "cad" "cay" "day"))  '("" 0))