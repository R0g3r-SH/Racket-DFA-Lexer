#lang racket/base
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(require "./txt.rkt")



(define calc-lexer
  (lexer

   [(:+ (:or (:+ #\- ) (:+ (char-range #\0 #\9))) (:+ #\E #\. #\e ) (:+ (char-range #\0 #\9)))
    ; =>
    (cons `(,(string->symbol lexeme))
          (calc-lexer input-port))]


   [(:+ (:or (char-range #\a #\z) (:+ #\_ ) (char-range #\0 #\9) (char-range #\A #\Z)))
    ; =>
    (cons `(,(string->symbol lexeme))
          (calc-lexer input-port))]


   [#\(
    ; =>
    (cons '(LPAR)
          (calc-lexer input-port))]

   [#\)
    ; =>
    (cons '(RPAR)
          (calc-lexer input-port))]



   [#\^
    ; =>
    (cons '(^)
          (calc-lexer input-port))]

    [#\/
    ; =>
    (cons '(/)
          (calc-lexer input-port))]

    
    [#\:
    ; =>
    (cons '(:)
          (calc-lexer input-port))]


   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    ; =>
    (cons (string->symbol lexeme)
          (calc-lexer input-port))]

   [(:or #\+ #\* #\=)
    ; =>
    (cons `( ,(string->symbol lexeme))
          (calc-lexer input-port))]

   [whitespace
    ; =>
    (calc-lexer input-port)]



  [(eof)
    '()]


  [(:: "//" (:*(complement (:or (char-range #\a #\z) #\, #\$ #\# #\% #\^ #\( #\)(char-range #\0 #\9) " " (char-range #\A #\Z) (:+ #\_ )) )) (:or #\space "\n" ""))
    ; =>
    (cons `(,(string->symbol lexeme))
          (calc-lexer input-port))]

  ))


;(calc-lexer (open-input-string "radio0=3. //cualquier secuencia de caracteres, como: lkas93jf=()792$#%7"))

(provide (all-defined-out))