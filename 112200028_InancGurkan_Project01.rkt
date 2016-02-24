#lang plai-typed

;Type for numbers and operations
(define-type Num
  [num (n : number)] ; Returns number itself
  [add (lhs : Num) (rhs : Num)] ; Adds two Nums
  [sum (lhs : Num) (rhs : Num)] ; Sums the rhs(Num) from the lhs(Num)
  [mul (lhs : Num) (rhs : Num)] ; Multiplies two Nums
  [pow (base : Num) (top : Num)] ; Takes base(Num) to power of top(Num)
  )

;Parses the s-expression to be evaluated.
(define (parse [s : s-expression]) : Num
  (cond
    [(s-exp-number? s) (num (s-exp->number s))]
    [(s-exp-list? s) (parseList s)]))

;Helper function of parse
;Parses the listof s-expression, evaluates the operation
(define (parseList [s : s-expression])
  (let ([exprList (s-exp->list s)]) ;Makes list from s-expressions
    (cond
      [(string=? (parseSymbol exprList) "+") (add (parse (second exprList)) (parse (third exprList)))]
      [(string=? (parseSymbol exprList) "-") (sum (parse (second exprList)) (parse (third exprList)))]
      [(string=? (parseSymbol exprList) "*") (mul (parse (second exprList)) (parse (third exprList)))]
      [(string=? (parseSymbol exprList) "**") (pow (parse (second exprList)) (parse (third exprList)))]
      )))

;Helper function of parseList
;Evaluates symbol in this step
(define (parseSymbol l) (symbol->string (s-exp->symbol (first l))))

;Accumulator approach to exponentiation operation
(define (takePower [b : number] [p : number] [cur : number])
  (cond
    [(= p 0) cur]
    [(> p 0) (takePower b (- p 1) (* b cur))]
    [else (error 'takePower "Illegal Expression")]))

;Evaluation function
(define (eval [s : s-expression])
  (let ([exprList (s-exp->list s)])
    (cond
      [(= (length exprList) 1) (s-exp->number (first exprList))]
      [else (evalHelper (parse s))])))

;Helper function of eval
;Operations are handled here
(define (evalHelper [expr : Num])
  (type-case Num expr
    [num (n) n]
    [add (lhs rhs) (+ (evalHelper lhs) (evalHelper rhs))]
    [sum (lhs rhs) (- (evalHelper lhs) (evalHelper rhs))]
    [mul (lhs rhs) (* (evalHelper lhs) (evalHelper rhs))]
    [pow (base top) (takePower (evalHelper base) (evalHelper top) 1)]))

;Function Tests
(test (takePower 2 3 1) 8)
(test (takePower 4 0 1) 1)
(test (takePower -2 3 1) -8)
;Test Examples
(define ex1 '(7))
(define ex2 '(+ 7 3))
(define ex3 '(+ (- 6 4) (* 2 7)))
(define ex4 '(** 2 3))
(define ex5 '(** (+ 2 3) (- 3 1)))
;Test Answers
(define ans1 7)
(define ans2 10)
(define ans3 16)
(define ans4 8)
(define ans5 25)
;Test cases
(test (eval ex1) ans1)
(test (eval ex2) ans2)
(test (eval ex3) ans3)
(test (eval ex4) ans4)
(test (eval ex5) ans5)