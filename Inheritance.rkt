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


(defmac (Class extends superclass
               ([field f init] ...)
               ([method m params body] ...))
  #:keywords  field method extends
  #:captures  self Super ! ?
  (let* ([scls superclass]
         [fields (append (scls 'all-fields)          ;(<super> <this>)
                         (list (cons 'f init) ...))] ;make sure that in a class (field a ax) (field a bx) get bx
         [methods
          (local [(defmac (? fd) #:captures self
                    (vector-ref (object-value self)                              
                                (find-last 'fd fields)))                
                  (defmac (! fd v) #:captures self                  
                    (vector-set! (object-value self)                               
                                 (find-last 'fd fields)                               
                                 v))
                  (defmac (Super md . args) #:captures self
                    (((scls 'lookup 'md) self) . args))]
            (list (cons 'm (λ (self)
                             (λ params body))) ...))])
    (letrec ([class
                 (lambda (msg . vals)
                   (case msg
                     [(all-fields) fields]
                     
                     [(create) (let ([vals (car vals)])
                                 (if (null? vals)
                                     (make-object class
                                       (list->vector (map cdr fields)))
                                     
                                     (let ([obj (make-object class
                                                  (list->vector (map cdr fields)))])
                                       (apply ((cdr (assoc 'initialize methods))
                                               obj) ;with user-def init-fun
                                              vals)
                                       obj)))]
                      
                     [(invoke) (let ((method (class 'lookup (second vals))))                           
                                 (apply (method (first vals)) (cddr vals)))]

                     [(lookup) (let ([found (assoc (first vals) methods)])                           
                                 (if found                               
                                     (cdr found)                               
                                     (scls 'lookup (first vals))))]))])
      class)))

(define-struct object (class value))

(define (new class . init-vals)
  (class 'create init-vals))

(defmac (Call o m arg ...)
  (let ([obj o])
    ((((object-class obj) 'lookup 'm) obj) arg ...)))

(define Root
  (λ (msg . vals)
    (case msg
      [(lookup)      (error "message not understood:" (first vals))]
      [(all-fields) '()]
      [else          (error "root class: should not happen: " msg)])))

(define (find-first fname field-names)
  (cond
    [(null? field-names) (error "no such field: " fname)]
    [(equal? (car field-names) fname) 1]
    [else (+ 1 (find-first fname
                           (cdr field-names)))]))

(define (find-last fname field)
  (let* ([len (length field)]
         [field-names (reverse (map car field))]
         [loc (find-first fname field-names)])
    (- len loc))) ;len - loc -1 +1


(define Point
  (Class extends Root
         ([field x 0] 
          [field y 0])
         ([method initialize (nx ny) (begin (! x nx) (! y ny))] 
          [method x? () (? x)]
          [method y? () (? y)]
          [method x! (nx) (! x nx)]
          [method y! (ny) (! y ny)])))

(define ColorPoint
  (Class extends Point
         ([field color 'black])
         ([method initialize (nc nx ny) (begin
                                          (! color nc)
                                          (Super x! nx)
                                          (Super y! ny))]
          [method color? () (? color)])))

(define p1 (new Point))
(define p2 (new Point 1 2))

(define rp (new ColorPoint 'red 2 3))


