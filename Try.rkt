#lang racket

(define-syntax defmacro
  (lambda (x)
    (syntax-case x ()
      ((define-macro (name . params) body1 body2 ...)
       #'(define-macro name (lambda params body1 body2 ...)))
      ((define-macro name expander)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x ()
               [(name . args) ;(_ . args) more hygiene!!
                (datum->syntax #'name (apply expander (syntax->datum #'args)))])))))))


(defmacro (nil! var)
  `(set! ,var empty))

(defmacro (def var val)
  `(define ,var ,val))


(defmacro (when test . do)
  `(if ,test
       (do ,@do)
       empty))

(defmacro (do . seq)
  `(begin ,@seq))

(defmacro (Let binds . body)
  `((lambda ,(map (lambda (x) (car x))
                  binds)
    ,@body)
  ,@(map (lambda (x) (cadr x))
        binds)))

(defmacro (letcond binds . body)
  `((lambda ,(map (lambda (x) (car x))
                  binds)
    ,@body)
  ,@(map (lambda (x)
           (if (not (null? (cdr x))) ;; var val
               (if (not (null? (cdr (cdr x))))  ;; var val if ...
                   (if (eval (car (cdr (cdr (cdr x)))))
                       (cadr x)
                       '())
                   (cadr x))
               '()))
        binds)))


(define-syntax (syntax-rules! x)
  (syntax-case x ()
    [(syntax-rules! () (pat syn))
     #'(lambda (ig)
         (syntax-case ig ()
           (pat #'syn)))]))

;(define-syntax let1
;  (syntax-rules! ()
;                 [(let1 (var val) body)
;                  ((lambda (var) body) val)]))







