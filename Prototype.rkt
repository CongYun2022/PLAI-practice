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

(defmac (Object parent
                ([field fname finit] ...)
                ([method mname para body] ...))
  #:keywords field method
  #:captures self
  (let ([fname finit] ...)
              (let ([methods (list (cons 'mname (lambda (self) (lambda para body))) ...)])
                (lambda (current) 
                  (lambda (msg . args)
                    (let ([found (assoc msg methods)])
                      (if found
                          (apply ((cdr found) current) args)
                          (apply (parent current) msg args))))))))

(defmac (Call obj msg args ...)
  (let ([obj* obj])
    ((obj* obj*) msg args ...)))

(define root
  (lambda (current)
    (lambda (msg . args)
      (error "no method can respand to: " msg))))