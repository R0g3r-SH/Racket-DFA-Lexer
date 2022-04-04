#lang racket
;https://books.google.com.mx/books?id=CKPTDwAAQBAJ&pg=PA262&lpg=PA262&dq=racket+DFA&source=bl&ots=GVghQT9NrS&sig=ACfU3U21MdCc8iccoteeHa-8IJkHRKS62w&hl=es&sa=X&ved=2ahUKEwib66Oz4_H2AhVtmWoFHe4xCM4Q6AF6BAgsEAM#v=onepage&q=racket%20DFA&f=false

(define state-table
  (vector
   ;       H  E  L  O
   (vector 1 #f #f #f) ;state 0
   (vector #f 2 #f #f)
   (vector #f #f 3 #f)
   (vector #f #f 3 4 )
   (vector #f #f #f 4) ;state 4
   ))

(define chr->ndx
  (make-hash'[(#\H . 0)(#\E . 1)(#\L . 2)(#\O . 3)]))

(define (next-state i chr)
  (if (hash-has-key? chr->ndx chr)
      (vector-ref (vector-ref state-table i)
                  (hash-ref chr->ndx chr))
      #f))

(define (hello-dfa str)
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

(hello-dfa "HEELLOOOO")