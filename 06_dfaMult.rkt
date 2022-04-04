#lang racket

(define state-table6
  (vector
  ;       =  !=
  (vector 1 #f ) ;state 0 
  ))

(define chr->ndx6
  (make-hash'[(#\* . 0)]))

(define (next-state6 i chr)
  (if (hash-has-key? chr->ndx6 chr)
      (vector-ref (vector-ref state-table6 i)
                  (hash-ref chr->ndx6 chr))
      #f))

(define (dfa_mult str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
        (if(= state 1)
            #t
            #f
            )
        (let ([state(next-state6 state (car chrs))]
              [tail (cdr chrs)])
          (if (equal? state #f) #f
              (loop state tail)
              ))))))

;(dfa_Asig "*")
(provide (all-defined-out))