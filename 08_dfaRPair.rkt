#lang racket

(define state-table8
  (vector
   ;       R  P  A R
   (vector 1 #f #f ) ;state 0
   (vector #f 2 #f )
   (vector #f #f 3 )
   (vector 4 #f #f )
   ))

(define chr->ndx8
  (make-hash'[(#\R . 0) (#\P . 1) (#\A . 2) ]))

(define (next-state8 i chr)
  (if (hash-has-key? chr->ndx8 chr)
      (vector-ref (vector-ref state-table8 i)
                  (hash-ref chr->ndx8 chr))
      #f))

(define (dfa_RPAIR str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
         (if(= state 4)
            #t
            #f
            )
         (let ([state(next-state8 state (car chrs))]
               [tail (cdr chrs)])
           (if (equal? state #f) #f
               (loop state tail)
               ))))))

;(dfa_RPAIR "RPAR")
(provide (all-defined-out))