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


(defmac (Class ([field f init] ...)
               ([method m params body] ...))
  #:keywords  field method
  #:captures  self ! ?
  (let ([methods
         (local [(defmac (? fd) #:captures self
                   (dict-ref (object-value self)
                             'fd))
                 (defmac (! fd v) #:captures self
                   (dict-set! (object-value self)
                              'fd v))]
           (list (cons 'm (λ (self)
                            (λ params body))) ...))])
    (letrec ([class
                 (λ (msg . vals)
                    (case msg
                     [(create) (if (null? vals)
                                   (make-object class
                                             (make-hash (list (cons 'f init) ...))) ;with init values
                                   
                                   (let ((obj (make-object class (make-hash))))
                                     (apply ((cdr (assoc 'initialize methods)) obj) ;with user-def init-fun
                                            vals)
                                     obj))]
                      
                     [(invoke) (if (assoc (second vals) methods)
                                   (apply ((cdr (assoc (second vals) methods))
                                           (first vals))
                                          (cddr vals))
                                   (error "message not understood"))]))])
      class)))

(define-struct object (class value))

(define (new class . init-vals)
  (apply class 'create init-vals))

(defmac (Call o m arg ...)
  (let ((obj o))
    ((object-class obj) 'invoke obj 'm arg ...)))