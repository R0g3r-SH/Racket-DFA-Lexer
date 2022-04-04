#lang racket

(define state-table22
  (vector
  ;       =  !=
  (vector 1 #f ) ;state 0 
  ))

(define chr->ndx22
  (make-hash'[(#\^ . 0)]))

(define (next-state22 i chr)
  (if (hash-has-key? chr->ndx22 chr)
      (vector-ref (vector-ref state-table22 i)
                  (hash-ref chr->ndx22 chr))
      #f))

(define (dfa_Pow str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
        (if(= state 1)
            #t
            #f
            )
        (let ([state(next-state22 state (car chrs))]
              [tail (cdr chrs)])
          (if (equal? state #f) #f
              (loop state tail)
              ))))))

;(dfa_Asig "^")
(provide (all-defined-out))