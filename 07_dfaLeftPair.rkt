#lang racket

(define state-table7
  (vector
   ;       L  P  A R
   (vector 1 #f #f #f) ;state 0
   (vector #f 2 #f #f)
   (vector #f #f 3 #f)
   (vector #f #f #f 4)
   ))

(define chr->ndx7
  (make-hash'[(#\L . 0) (#\P . 1) (#\A . 2) (#\R . 3)]))

(define (next-state7 i chr)
  (if (hash-has-key? chr->ndx7 chr)
      (vector-ref (vector-ref state-table7 i)
                  (hash-ref chr->ndx7 chr))
      #f))

(define (dfa_LPAIR str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
         (if(= state 4)
            #t
            #f
            )
         (let ([state(next-state7 state (car chrs))]
               [tail (cdr chrs)])
           (if (equal? state #f) #f
               (loop state tail)
               ))))))

;(dfa_LPAIR "LPAR")
(provide (all-defined-out))