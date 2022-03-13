#lang racket

(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax (defmac stx)
  (syntax-parse stx
    [(defmac (name:identifier . xs) 
       (~optional (~seq #:keywords key:identifier ...) #:defaults ([(key 1) '()]))
       (~optional (~seq #:captures cap:identifier ...) #:defaults ([(cap 1) '()]))
       body:expr)
     #'(define-syntax (name stx)
         (syntax-case stx (key ...)
           [(name . xs)
            (with-syntax ([cap (datum->syntax stx 'cap stx)] ...)
              (syntax body))]))]))

(defmac (Object ([field fname finit] ...)
                ([method mname para body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname finit] ...)
              (let ([methods (list (cons 'mname (lambda para body)) ...)])
                (lambda (msg . args)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) args)
                        (error "message not understand: " msg))))))])
    self))

(defmac (Call obj msg args ...)
  (obj 'msg args ...))



