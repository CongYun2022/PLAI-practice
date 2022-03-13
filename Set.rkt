#lang plai-typed


(define-type Expr
  [Nmb (n : number)]
  [Add (l : Expr) (r : Expr)]
  [Seq (f : Expr) (n : Expr)] 
  [Var (idf : symbol)]      
  [Lambda (para : (listof symbol)) (body : Expr)]
  [Apply (fun : Expr) (arg : (listof Expr))]
  
  [Set (idf : symbol) (val : Expr)]
  [MkBox (val : Expr)]
  [UnBox (box : Expr)]
  [SetBox (box : Expr) (val : Expr)]

  [If (test : Expr) (texpr : Expr) (fexpr : Expr)]
  [Eq (l : Expr) (r : Expr)]
  
  [Cons (l : Expr) (r : Expr)]
  [Car (p : Expr)]
  [Cdr (p : Expr)]
  [SetCar (p : Expr) (val : Expr)]
  [SetCdr (p : Expr) (val : Expr)]
  [Nil])


(define-type Value
  [Val (n : number)]
  [Closure (paras : (listof symbol)) (body : Expr) (env : Envrionment)]
  [Loc (loc : Location)]
  
  [Pair (aloc : Location) (dloc : Location)]
  [None]
  
  [True] [False])


(define-type Result
  [VS (val : Value) (sto : Store)])


(define-type-alias Location number)

(define new-loc
  (let ([x 0])
    (lambda ()(begin (set! x (+ 1 x))
                     x))))



(define-type Binding
  [Bind (var : symbol) (loc : Location)])

(define (lookup [var : symbol] [env : Envrionment]) : Location
  (cond
    [(empty? env) (error 'lookup "No such binding")]
    [(equal? var (Bind-var (first env)))
     (Bind-loc (first env))]
    [else   (lookup var (rest env))]))


(define-type-alias Envrionment (listof Binding))
(define extend-env cons)
(define globle-env empty)
 


(define-type Storage
  [Point (loc : Location) (val : Value)])


(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "No such storage")]
    [(equal? loc (Point-loc (first sto))) 
     (Point-val (first sto))]
    [else   (fetch loc (rest sto))]))

(define (fresh-sto [loc : Location] [val : Value] [sto : Store]) : Store
  (cond
    [(empty? sto) empty]
    [(equal? loc (Point-loc (first sto)))
     (cons (Point loc val)
           (rest sto))] 
    [else (cons (first sto)
                (fresh-sto loc val (rest sto)))]))


(define-type-alias Store (listof Storage))
(define override-sto cons)
(define init-store empty)





(define (prase [expr : s-expression]) : Expr
  (cond
    [(s-exp-number? expr) (Nmb (s-exp->number expr))]
    [(s-exp-symbol? expr) (let ([idf (s-exp->symbol expr)])
                            (if (equal? idf 'nil)
                                (Nil)
                                (Var idf)))]
    [(s-exp-list? expr)
     (let ([expr-list (s-exp->list expr)])
       (cond
         [(s-exp-list? (first expr-list))
          (Apply (prase (first  expr-list))
                 (map (lambda (arg) (prase arg))
                      (rest expr-list)))]
         [else (case (s-exp->symbol (first expr-list))
                 [(lambda) (Lambda (map (lambda (para) (s-exp->symbol para))
                                        (s-exp->list (second expr-list))) 
                                   (prase (third expr-list)))]
                 [(+)      (Add (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(if)     (If  (prase (second expr-list))
                                (prase (third  expr-list))
                                (prase (fourth  expr-list)))]

                 [(eq?)    (Eq  (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(begin)  (Seq (prase (second expr-list))
                                (prase (third  expr-list)))]
                 [(let)    (let ([bind (s-exp->list (second expr-list))]
                                 [body   (prase (third expr-list))])
                            (let ([idf  (s-exp->symbol (first bind))]
                                  [val (prase (second bind))])
                              (Apply (Lambda (list idf) body)
                                     (list val))))]
                 [(letrec)  (let ([bind (s-exp->list (second expr-list))]
                                  [body   (prase (third expr-list))])
                             (let ([idf  (s-exp->symbol (first bind))]
                                  [val (prase (second bind))])
                              (Apply (Lambda (list idf) 
                                             (Seq (Set idf val)
                                                  body))
                                     (list (Nmb 555)))))]
                                             
                 ;;box&set
                 [(box)     (MkBox   (prase (second expr-list)))]
                 [(unbox)   (UnBox   (prase (second expr-list)))]
                 [(setbox!) (SetBox  (prase (second expr-list))
                                     (prase (third  expr-list)))]
                 [(set!)    (Set     (s-exp->symbol (second expr-list))
                                     (prase (third  expr-list)))]

                 [(cons)    (Cons    (prase (second expr-list))
                                     (prase (third  expr-list)))]
                 [(car)     (Car     (prase (second expr-list)))]
                 [(cdr)     (Cdr     (prase (second expr-list)))]
                 [(setcar!) (SetCar  (prase (second expr-list))
                                     (prase (third  expr-list)))]
                 [(setcdr!) (SetCdr  (prase (second expr-list))
                                     (prase (third  expr-list)))]
                 
                 [(list)    (let ([vals (map (lambda (expr) (prase expr))
                                             (rest expr-list))])
                              (letrec ([delist (lambda (li)
                                                (cond 
                                                  [(empty? li) (Nil)] 
                                                  [else   (Cons (first li)
                                                                (delist (rest li)))]))])
                                (delist vals)))]
                 
                 [else       (Apply (prase (first  expr-list))
                                    (map (lambda (arg) (prase arg))
                      				     (rest expr-list)))])]))]))




(define (interp [expr : Expr] [env : Envrionment] [sto : Store]) : Result
  (type-case Expr expr
    [Nmb (n) (VS (Val n) sto)]
    [Add (l r) (type-case Result (interp l env sto)
                 [VS (lval lsto) (type-case Result (interp r env lsto)
                                   [VS (rval rsto) (VS (add rval lval)
                                                       rsto)])])]
    [Seq (l r) (type-case Result (interp l env sto)
                 [VS (lval lsto) (type-case Result (interp r env lsto)
                                   [VS (rval rsto) (VS rval rsto)])])]
    [Var (idf) (VS (fetch (lookup idf env) sto) sto)]
    [Lambda (paras body) (VS (Closure paras body env) sto)]
    [Apply (fun args) (type-case Result (interp fun env sto)
                        [VS (fval fsto) (let ([sto fsto])
                                          (let ([avals (map (lambda (arg) (type-case Result (interp arg env sto)
                                                                            [VS (aval asto) (begin (set! sto asto)
                                                                                                   aval)]))
                                                            args)])
                                            (begin (map2 (lambda (para arg) (let ([where (new-loc)])
                                                                              (begin
                                                                                (set! env (extend-env (Bind para
                                                                                                            where)
                                                                                                      env))
                                                                                (set! sto (override-sto (Point where
                                                                                                               arg)
                                                                                                                    sto)))))
                                                         (Closure-paras fval) avals)
                                                   (interp (Closure-body fval)
                                                           env
                                                           sto))))])]
    [Set (idf val) (type-case Result (interp val env sto)
                     [VS (vval vsto) (let ([loc (lookup idf env)])
                                       (VS vval
                                           (fresh-sto loc vval vsto)))])]
    [MkBox (val)  (type-case Result (interp val env sto)
                    [VS (vval vsto) (let ([where (new-loc)])
                                      (VS (Loc (Val-n vval))
                                          (override-sto (Point where vval)
                                                        vsto)))])]
    [UnBox (box) (type-case Result (interp box env sto)
                   [VS (bval bsto) (VS (fetch (Val-n bval) bsto)
                                       bsto)])]
    [SetBox (box val) (type-case Result (interp box env sto)
                        [VS (bval bsto) (type-case Result (interp val env bsto)
                                          [VS (vval vsto) (VS vval
                                                              (fresh-sto (Val-n bval) vval vsto))])])]
             
    [If (test t f) (type-case Result (interp test env sto)
                     [VS (tval tsto) (type-case Value tval
                                       [True ()  (interp t env tsto)]
                                       [False () (interp f env tsto)]
                                       [else     (error 'interp "if test expr must be a boolen")])])]
    [Eq (l r) (type-case Result (interp l env sto)
                [VS (lval lsto) (type-case Result (interp r env lsto)
                                  [VS (rval rsto) (if (eq? (Val-n lval)
                                                           (Val-n rval))
                                                      (VS (True) rsto)
                                                      (VS (False) rsto))])])]
    [Nil () (VS (None) sto)]
             
    [Cons (a d) (type-case Result (interp a env sto)
                  [VS (aval asto) (type-case Result (interp d env asto)
                                    [VS (dval dsto) (let ([aloc (new-loc)]
                                                          [dloc (new-loc)])
                                                      (begin (set! dsto (override-sto (Point aloc aval) dsto))
                                                             (set! dsto (override-sto (Point dloc dval) dsto))
                                                             (VS (Pair aloc dloc)
                                                                 dsto)))])])]
    [Car (p)    (type-case Result (interp p env sto)
                  [VS (pval psto) (VS (fetch (Pair-aloc pval) psto)
                                      psto)])]
    [Cdr (p)    (type-case Result (interp p env sto)
                  [VS (pval psto) (VS (fetch (Pair-dloc pval) psto)
                                      psto)])]
    [SetCar (p val) (type-case Result (interp p env sto)
                      [VS (pval psto) (type-case Result (interp val env psto)
                                        [VS (vval vsto) (VS vval
                                                            (fresh-sto (Pair-aloc pval) vval vsto))])])]
    [SetCdr (p val) (type-case Result (interp p env sto)
                      [VS (pval psto) (type-case Result (interp val env psto)
                                        [VS (vval vsto) (VS vval
                                                            (fresh-sto (Pair-dloc pval) vval vsto))])])]))

(define (add [l : Value] [r : Value]) : Value
  (Val (+ (Val-n l)
          (Val-n r))))


(define (devalue [val : Value] [sto : Store])
    (type-case Value val
      [Val  (nmb)  (number->s-exp nmb)]
      [Loc  (loc) `(val: ,(devalue (fetch loc sto) sto) at: ,(number->s-exp loc))]
      [True  ()    (symbol->s-exp 'True)] 
      [False ()    (symbol->s-exp 'False)]  
      [Closure (para body env) (symbol->s-exp '<Colsure>)]
      [Pair (aloc dloc)       `(,(devalue (fetch aloc sto) sto)  ,(devalue (fetch dloc sto) sto))]
      [None ()                 (symbol->s-exp 'nil)])) 


(define (run [expr : s-expression])
	(type-case Result (interp (prase expr) globle-env init-store)
	   [VS (val sto) 
     	(devalue val sto)]))
