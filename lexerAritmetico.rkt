#lang racket

(require "./01_dfaAsig.rkt")
(require "./02_dfaEntero.rkt")
(require "./03_dfaReal.rkt")
(require "./04_dfaVariable.rkt")
(require "./05_dfaComentario.rkt")
(require "./lex.rkt")
(require "./06_dfaMult.rkt")
(require "./07_dfaLeftPair.rkt")
(require "./08_dfaRPair.rkt")
(require "./09_dfa_Rest.rkt")
(require "./10_dfaDiv.rkt")
(require "./11_dfa_Pow.rkt")
(require "./12_dfa_ERR.rkt")
(require 2htdp/batch-io)
(require "./txt.rkt")


(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
;(define in (calc-lexer (open-input-string fdata)))
;(define in (calc-lexer (open-input-string "expresion83r34   = 34+interes-diametro4*.93    + (34+21.e2)" )))

(define (main str )
  (cond
    [( empty? str)'() ]
    [(dfa_Asig (symbol->string(car(car str)))) (cons `(Asignacion,(car (car str))) (main (cdr str)) ) ]
    [(dfa_mult (symbol->string(car(car str)))) (cons `(Multiplicación,(car (car str))) (main (cdr str)) ) ]
    [(dfa_resta (symbol->string(car(car str)))) (cons `(Resta,(car (car str))) (main (cdr str)) ) ]
    [(dfa_Div (symbol->string(car(car str)))) (cons `(División,(car (car str))) (main (cdr str)) ) ]
    [(dfa_ERR (symbol->string(car(car str)))) (cons `(valido no Caracter,(symbol->string(car (car str)))) (main (cdr str)) ) ]
    [(dfa_Pow (symbol->string(car(car str)))) (cons `(Potencia,(car (car str))) (main (cdr str)) ) ]
    [(dfa_LPAIR (symbol->string(car(car str)))) (cons `(abre que Paréntesis ,(car (car str))) (main (cdr str)) )]
    [(dfa_RPAIR (symbol->string(car(car str)))) (cons `(cierra que Paréntesis ,(car (car str))) (main (cdr str)) )]
    [(dfa_n_real (symbol->string(car(car str)))) (cons `(Real, (string->number(symbol->string(car (car str))))) (main (cdr str)) ) ]
    [(intdfa (symbol->string(car(car str)))) (cons `(Entero, (string->number(symbol->string(car (car str))))) (main (cdr str)) ) ]
    [(var-dfa(symbol->string(car(car str)))) (cons `(Variable,(car (car str))) (main (cdr str)) ) ]
    [(coment-dfa(symbol->string(car(car str)))) (cons `(Comentario,(symbol->string(car (car str)))) (main (cdr str)) ) ]
    
    [else (main (cdr str))]
    ))


(define (simplify list)
  (cond
    [(= 1 (length list))(append (reverse (car list)) (cdr list))]
    [(append (reverse (car list)) (simplify (cdr list)))]
    )
  )

;(simplify (main in))
;(simplify(main(calc-lexer(open-input-string (sixth data)))))

(define (simplify2 data)
  (cond
    [(string-ci=? (car data) "") (simplify2(cdr data))]
    [(= 1 (length data)) (append (simplify (main (calc-lexer(open-input-string (car data)) )) ) (cdr data))]
    [(append (simplify (main(calc-lexer(open-input-string (car data))))) (simplify2 (cdr data)))]
    )
  )


(simplify2 data)
(define outtxt (simplify2 data))

;(string-join (map ~a outtxt) " ")
(write-file "output.txt" (string-join (map ~a outtxt) ""))
(write-file "output_oneliner.txt" (string-join (map ~a outtxt) "\n"))