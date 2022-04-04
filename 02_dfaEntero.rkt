#lang racket

(define state-table2
  (vector
   ;       - 0 1 2 3 4 5 6 7 8 9
   (vector 1 1 1 1 1 1 1 1 1 1 1)
   (vector #f 1 1 1 1 1 1 1 1 1 1) ;state 0 
   (vector #f 1 1 1 1 1 1 1 1 1 1)
   ))

(define chr->ndx2
  (make-hash'[(#\- .  0)(#\0 . 1)(#\1 . 2)(#\2 . 3)(#\3 . 4)(#\4 . 5)(#\5 . 6)(#\6 . 7)(#\7 . 8)(#\8 . 9) (#\9 . 10)]))

(define (next-state2 i chr)
  (if (hash-has-key? chr->ndx2 chr)
      (vector-ref (vector-ref state-table2 i)
                  (hash-ref chr->ndx2 chr))
      #f))

(define (intdfa str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
         (if(= state 1)
            #t
            #f
            )
         (let ([state(next-state2 state (car chrs))]
               [tail (cdr chrs)])
           (if (equal? state #f) #f
               (loop state tail)
               ))))))


;(intdfa "-20")
(provide (all-defined-out))