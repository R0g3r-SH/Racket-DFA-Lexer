#lang racket

(define state-table9
  (vector
  ;       =  !=
  (vector 1 #f ) ;state 0 
  ))

(define chr->ndx9
  (make-hash'[(#\- . 0)]))

(define (next-state9 i chr)
  (if (hash-has-key? chr->ndx9 chr)
      (vector-ref (vector-ref state-table9 i)
                  (hash-ref chr->ndx9 chr))
      #f))

(define (dfa_resta str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
        (if(= state 1)
            #t
            #f
            )
        (let ([state(next-state9 state (car chrs))]
              [tail (cdr chrs)])
          (if (equal? state #f) #f
              (loop state tail)
              ))))))

;(dfa_Asig "-")
(provide (all-defined-out))