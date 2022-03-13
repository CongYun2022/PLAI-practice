#lang plai

(define (immediate  pred?)
  (lambda (val)
    (if (pred? val)
        val
        (blame "violation"))))

(define (blame s)
  (error 'contract "~a" s))


(define (function dom rng)
  (lambda (fun)
    (if (procedure? fun)
        (lambda (x)
          (rng (fun (dom x))))
        (blame fun))))

(define-syntax (define/contract stx)
  (syntax-case stx (::)
    [(_ (f (id :: c) ...) b)
     (with-syntax ([(new-id ...) (generate-temporaries #'(id ...))])
       #'(define f
           (lambda (new-id ...)
             (let ([id (guard c new-id)]
                   ...)
               b))))]))
