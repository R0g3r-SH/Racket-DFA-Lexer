#lang racket

(define state-table11
  (vector
   ;       /  /
   (vector 1 #f )
   (vector #f #f ) 
   (vector #f #f ) ;state 0
   ))

(define chr->ndx11
  (make-hash'[(#\/ . 0)]))

(define (next-state11 i chr)
  (if (hash-has-key? chr->ndx11 chr)
      (vector-ref (vector-ref state-table11 i)
                  (hash-ref chr->ndx11 chr))
      #f))

(define (dfa_Div str)
  (let ([chrs (string->list str)])
    (let loop ([state 0][chrs chrs])
      (if(equal? chrs '())
         (if(= state 1)
            #t
            #f
            )
         (let ([state(next-state11 state (car chrs))]
               [tail (cdr chrs)])
           (if (equal? state #f) #f
               (loop state tail)
               ))))))

;(dfa_Div "//cualquier secuencia de caracteres, como: lkas93jf=()792$#%7")
(provide (all-defined-out))