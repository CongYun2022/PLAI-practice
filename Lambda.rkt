#lang plai-typed

(define-type Expr
  [Nmb (n : number)]
  [Add (l : Expr) (r : Expr)]
  [Mul (l : Expr) (r : Expr)]
  [Idf (x : symbol)]
  [Lambda (para : symbol) (body : Expr)]
  [Apply (fun : Expr) (arg : Expr)]
  [If (test : Expr) (t : Expr) (f : Expr)]
  [Eq (l : Expr) (r : Expr)])


(define-type Value
  [Val (n : number)]
  [Closure (para : symbol) (body : Expr) (env : Envrionment)]
  [True] [False])


(define-type Binding
  [Bind (idf : symbol) (val : Value)])

(define (lookup [idf : symbol] [env : Envrionment]) : Value
  (cond
    [(empty? env) (error 'lookup "no such idf")]
    [(equal? idf (Bind-idf (first env))) (Bind-val (first env))]
    [else (lookup idf (rest env))]))


(define-type-alias Envrionment (listof Binding))

(define extend-env cons)
(define globle-env empty)




(define (prase [expr : s-expression]) : Expr
  (cond
    [(s-exp-number? expr) (Nmb (s-exp->number expr))]
    [(s-exp-symbol? expr) (Idf (s-exp->symbol expr))]
    [(s-exp-list? expr)
     (let ([expr-list (s-exp->list expr)])
       (cond
         [(s-exp-list? (first expr-list))
          (Apply (prase (first  expr-list))
                 (prase (second expr-list)))]
         [else (case (s-exp->symbol (first expr-list))
                 [(lambda) (Lambda (s-exp->symbol (second expr-list))
                                   (prase (third expr-list)))]
                 [(+)      (Add (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(*)      (Mul (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(eq?)      (Eq (prase (second expr-list))
                                 (prase (third  expr-list)))]
                 [(if)     (If  (prase (second expr-list))                             
                                (prase (third  expr-list))
                                (prase (fourth expr-list)))]
                 

                 [(let)   (let ([bind (s-exp->list (second expr-list))]
                                [body   (prase (third expr-list))])
                            (let ([idf  (s-exp->symbol (first bind))]
                                  [val (prase (second bind))])
                              (Apply (Lambda idf body)
                                     val)))]
                 [else     (Apply (prase (first  expr-list))
                                  (prase (second expr-list)))])]))]))




(define (interp [expr : Expr] [env : Envrionment]) : Value
  (type-case Expr expr
             [Nmb (n) (Val n)]
             [Add (l r) (add (interp l env)
                             (interp r env))]
             [Mul (l r) (mul (interp l env)
                             (interp r env))]
             [Idf (x) (lookup x env)]
             [Lambda (para body) (Closure para body env)]
             [Apply (fun arg) (let ([clo (interp fun env)])
                                (interp (Closure-body clo)
                                        (extend-env (Bind (Closure-para clo)
                                                           (interp arg env))
                                                     (Closure-env clo))))]
             [If (test t f) (type-case Value (interp test env)
                                       [True ()  (interp t env)]
                                       [False () (interp f env)]
                                       [else     (error 'interp "test-expr must be a boolen")])]
             [Eq (l r) (if (eq? (Val-n (interp l env))
                                (Val-n (interp r env)))
                           (True)
                           (False))]))



(define (add [l : Value] [r : Value]) : Value
  (Val (+ (Val-n l)
          (Val-n r))))

(define (mul [l : Value] [r : Value]) : Value
  (Val (* (Val-n l)
          (Val-n r))))





(define (run [s-expr : s-expression]) : Value
  (interp (prase s-expr) globle-env))
