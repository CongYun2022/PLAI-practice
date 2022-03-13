#lang plai-typed

(define-type Expr
  [Nmb (n : number)]
  [Add (l : Expr) (r : Expr)]
  [Mul (l : Expr) (r : Expr)])


(define (prase [expr : s-expression]) : Expr
  (cond
    [(s-exp-number? expr) (Nmb (s-exp->number expr))]
    [(s-exp-list? expr)
     (let ([expr-list (s-exp->list expr)])
          (case (s-exp->symbol (first expr-list))
                 [(+)      (Add (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(*)      (Mul (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [else (error 'prase "operator not support")]))]))

(define (interp [expr : Expr]) : number
  (type-case Expr expr
     [Nmb (n) n]
     [Add (l r) (+ (interp l)
                   (interp r))]
     [Mul (l r) (* (interp l)
                   (interp r))]))

(define (start [expr : s-expression])
  (interp (prase expr)))

