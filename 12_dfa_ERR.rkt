#lang racket

(define state-table33
  (vector
  ;       =  !=
  (vector 1 #f ) ;state 0 
  ))

(define chr->ndx33
  (make-hash'[(#\: . 0)]))

(define (next-state33 i chr)
  (if (hash-has-key? chr->ndx33 chr)
      (vector-ref (vector-ref state-table33 i)
                  (hash-ref chr->ndx33 chr))
      #f))

(define (dfa_ERR str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
        (if(= state 1)
            #t
            #f
            )
        (let ([state(next-state33 state (car chrs))]
              [tail (cdr chrs)])
          (if (equal? state #f) #f
              (loop state tail)
              ))))))

(dfa_ERR ":esdjtisgjiosdjigosdg")
(provide (all-defined-out))