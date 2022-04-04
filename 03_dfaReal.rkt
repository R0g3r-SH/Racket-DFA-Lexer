#lang racket

(define state-table
  (vector
   ;       + - 0 1 2 3 4 5 6 7 8 9  .  E  e
   (vector 1 1 1 1 1 1 1 1 1 1 1 1 3 #f #f) ;state 0
   (vector #f #f 2 2 2 2 2 2 2 2 2 2 4 #f #f);state 1
   (vector #f #f 3 3 3 3 3 3 3 3 3 3 3 3 3);state 2
   (vector #f #f 4 4 4 4 4 4 4 4 4 4 4 4 4);state 3
   (vector #f 4 4 4 4 4 4 4 4 4 4 4 4 4 4) ;state 4
   ))

(define chr->ndx
  (make-hash'[(#\+ .  0)(#\- . 1)(#\0 . 2)(#\1 . 3)(#\2 . 4)(#\3 . 5)(#\4 . 6)(#\5 . 7)(#\6 . 8)(#\7 . 9)(#\8 . 10) (#\9 . 11)(#\. . 12)(#\E . 13)(#\e . 14)]))

(define (next-state i chr)
  (if (hash-has-key? chr->ndx chr)
      (vector-ref (vector-ref state-table i)
                  (hash-ref chr->ndx chr))
      #f))

(define (dfa_n_real str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
         (if(= state 4)
            #t
            #f
            )
         (let ([state(next-state state (car chrs))]
               [tail (cdr chrs)])
           (if (equal? state #f) #f
               (loop state tail)
               ))))))

;(dfa_n_real "0.93")

(provide (all-defined-out))

;["a = 32.4 *(-8.6 - b)"]
;["a = 32.4 *","(", "-8.6 - b", ")"]
;["a", "=" , "32.4", "*", "(", "-8.6 - b", ")"]
