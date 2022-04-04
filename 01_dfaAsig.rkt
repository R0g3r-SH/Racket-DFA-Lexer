#lang racket

(define state-table1
  (vector
  ;       =  !=
  (vector 1 #f ) ;state 0 
  ))

(define chr->ndx1
  (make-hash'[(#\= . 0)]))

(define (next-state1 i chr)
  (if (hash-has-key? chr->ndx1 chr)
      (vector-ref (vector-ref state-table1 i)
                  (hash-ref chr->ndx1 chr))
      #f))

(define (dfa_Asig str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
        (if(= state 1)
            #t
            #f
            )
        (let ([state(next-state1 state (car chrs))]
              [tail (cdr chrs)])
          (if (equal? state #f) #f
              (loop state tail)
              ))))))

;(dfa_Asig "=")
(provide (all-defined-out))