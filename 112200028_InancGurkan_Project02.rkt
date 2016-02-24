#lang plai-typed

;Grammar for operations
;;; S1 = {+, -, *, **}
;;; S2 = [0-9]
;;; S = S1 S S | S2 | - S

;; Definitions for numbers and operations
(define-type myNum
  [num (n : number)] ; Returns the number itself.
  [add (lhs : myNum) (rhs : myNum)] ; Adds two myNums.
  [neg (n : myNum)] ; Returns negative n (-n).
  [mul (lhs : myNum) (rhs : myNum)] ; Multiplies two myNums.
  [pow (base : myNum) (top : myNum)] ; Takes base(myNum) to power of top(myNum).
  )

;; Definitions for myNumSug (Sugared)
(define-type myNumSug
  [numSug (n : number)] ; Returns the number itself.
  [addSug (lhs : myNumSug) (rhs : myNumSug)] ; Adds two myNumSugs.
  [subSug (lhs : myNumSug) (rhs : myNumSug)] ; Substracts rhs from lhs.
  [negSug (n : myNumSug)] ; Returns negative n (-n).
  [mulSug (lhs : myNumSug) (rhs : myNumSug)] ; Multiplies two myNumSugs.
  [powSug (base : myNumSug) (top : myNumSug)] ; Takes base(myNumSug) to power of top(myNumSug).
  )

;; desugar : myNumSug -> myNum
(define (desugar [ms : myNumSug]) : myNum
  (type-case myNumSug ms
    [numSug (n) (num n)]
    [addSug (lhs rhs) (add (desugar lhs) (desugar rhs))]
    [subSug (lhs rhs) (add (desugar lhs) (mul (desugar rhs) (num -1)))]
    [negSug (n) (mul (num -1) (desugar n))]
    [mulSug (lhs rhs) (mul (desugar lhs) (desugar rhs))]
    [powSug (base top) (pow (desugar base) (desugar top))]))

;; takePower : number number -> number
;; Takes base to the power of top using accumulator approach.
;; Examples
;; 5 3 -> 125
;; 3 2 -> 9
;; -2 4 -> 16
;; -2 5 -> -32
;; 0 4 -> 0
;; 4 -1 -> Illegal Expression
(define (takePower [base : number] [top : number]) : number
  (takePowerHelper base top 1))

;; takePowerHelper : number number number -> number
;; Accumulator approach implementation to takePower function
;; Examples are the same as takePower
;; Template
;; (define (takePowerHelper [b : number] [p : number] [cur : number]) : number
;;   (cond
;;     [(= p 0) ...]
;;     [(> p 0) (takePowerHelper ... ... ...)]
;;     [else ...]))
(define (takePowerHelper [b : number] [p : number] [cur : number]) : number
  (cond
    [(= p 0) cur]
    [(> p 0) (takePowerHelper b (- p 1) (* b cur))]
    [else (error 'takePower "Illegal Expression")]))

;;Take Power Tests
(test (takePowerHelper 5 3 1) 125)
(test (takePowerHelper 3 2 1) 9)
(test (takePowerHelper -2 4 1) 16)
(test (takePowerHelper -2 5 1) -32)
(test (takePowerHelper 0 4 1) 0)
(test (takePower 5 3) 125)
(test (takePower 3 2) 9)
(test (takePower -2 4) 16)
(test (takePower -2 5) -32)
(test (takePower 0 4) 0)


;; parseSug : s-expression -> mynumSug
;; Converts a s-expression to mynumSug
;; Examples
;; '4 -> (numSug 4)
;; '7 -> (numSug 7)
;; '12 -> (numSug 12)
;; '15 -> (numSug 15)
;; '20 -> (numSug 20)
;; '(+ 3 4) -> (addSug (numSug 3) (numSug 4))
;; '(+ 4 5) -> (addSug (numSug 4) (numSug 5))
;; '(+ 4 6) -> (addSug (numSug 4) (numSug 6))
;; '(+ 12 5) -> (addSug (numSug 12) (numSug 5))
;; '(+ 26 3) -> (addSug (numSug 26) (numSug 3))
;; '(- 4 3) -> (subSug (numSug 4) (numSug 3))
;; '(- 42 5) -> (subSug (numSug 42) (numSug 5))
;; '(- 4 6) -> (subSug (numSug 4) (numSug 6))
;; '(- 12 5) -> (subSug (numSug 12) (numSug 5))
;; '(- 26 3) -> (subSug (numSug 26) (numSug 3))
;; '(- 3) -> (negSug (numSug 3))
;; '(- 4) -> (negSug (numSug 4))
;; '(- -4) -> (negSug (numSug -4))
;; '(- 12) -> (negSug (numSug 12))
;; '(- 26) -> (negSug (numSug 26))
;; '(* 3 4) -> (mulSug (numSug 3) (numSug 4))
;; '(* 4 5) -> (mulSug (numSug 4) (numSug 5))
;; '(* 4 6) -> (mulSug (numSug 4) (numSug 6))
;; '(* 12 5) -> (mulSug (numSug 12) (numSug 5))
;; '(* 26 3) -> (mulSug (numSug 26) (numSug 3))
;; '(** 3 4) -> (powSug (numSug 3) (numSug 4))
;; '(** 4 5) -> (powSug (numSug 4) (numSug 5))
;; '(** 4 6) -> (powSug (numSug 4) (numSug 6))
;; '(** 12 5) -> (powSug (numSug 12) (numSug 5))
;; '(** 26 3) -> (powSug (numSug 26) (numSug 3))
;; '(+ 5 (* 6 3)) -> (addSug (numSug 5) (mulSug (numSug 6) (numSug 3)))
;; '(+ 25 (- (** 2 4))) -> (addSug (numSug 25) (negSug (powSug (numSug 2) (numSug 4))))
;; '(* 8 (+ 6 (+ 5 (- 3)))) -> (mulSug (numSug 8) (addSug (numSug 6) (addSug (numSug 5) (negSug (numSug 3)))))
;; '(* 0 (** -5 3)) -> (mulSug (numSug 0) (powSug (numSug -5) (numSug 3)))
;; '(** (+ 6 1) 8) -> (addSug (addSug (numSug 6) (numSug 1)) (numSug 8))
;; Template
;; (define (parse [s : s-expression] : mynumSug
;;   (cond
;;     [(s-exp-number? s) ...]
;;     [(s-exp-list? s) ...
;;       (let ...
;;         (case ...
;;           [(+) (addSug ... ...)]
;;           [(-) (minusHelper ...)]
;;           [(*) (mulSug ... ...)]
;;           [(**) (powSug ... ...)]
;;           [else ...]))]
;;     [else ...]))

(define (parseSug [s : s-expression]) : myNumSug
  (cond
    [(s-exp-number? s) (numSug (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (addSug (parseSug (second sl)) (parseSug (third sl)))]
         [(-) (minusHelper sl)]
         [(*) (mulSug (parseSug (second sl)) (parseSug (third sl)))]
         [(**) (powSug (parseSug (second sl)) (parseSug (third sl)))]
         [else (error 'parseSug "invalid list input")]))]
    [else (error 'parseSug "invalid input")]))

;; minusHelper : list of s-expression : myNumSug
;; Parses s-expressions with a - symbol.
;; Examples
;; (s-exp->list '(- 5))) -> (negSug (numSug 5))
;; (s-exp->list '(- 5 3))) -> (subSug (numSug 5) (numSug 3)))
;; (s-exp->list '(- 5 (- 3)))) -> (subSug (numSug 5) (negSug (numSug 3)))
;; (s-exp->list '(- 5 (- 3 6)))) -> (subSug (numSug 5) (subSug (numSug 3) (numSug 6)))
;; (s-exp->list '(- (- 5 3)))) (negSug (subSug (numSug 5) (numSug 3)))
;; Template
;; (define (minusHelper sl) : myNumSug
;;   (cond
;;     [(> (length sl) 2) (subSug ...(second sl) ...(third sl))]
;;     [else (negSug ...(second sl))]))
(define (minusHelper sl) : myNumSug
  (cond
    [(> (length sl) 2) (subSug (parseSug (second sl)) (parseSug (third sl)))]
    [else (negSug (parseSug (second sl)))]))

(test (minusHelper (s-exp->list '(- 5))) (negSug (numSug 5)))
(test (minusHelper (s-exp->list '(- 5 3))) (subSug (numSug 5) (numSug 3)))
(test (minusHelper (s-exp->list '(- 5 (- 3)))) (subSug (numSug 5) (negSug (numSug 3))))
(test (minusHelper (s-exp->list '(- 5 (- 3 6)))) (subSug (numSug 5) (subSug (numSug 3) (numSug 6))))
(test (minusHelper (s-exp->list '(- (- 5 3)))) (negSug (subSug (numSug 5) (numSug 3))))

(test (parseSug '4) (numSug 4))
(test (parseSug '7) (numSug 7))
(test (parseSug '12) (numSug 12))
(test (parseSug '15) (numSug 15))
(test (parseSug '20) (numSug 20))
(test (parseSug '(+ 3 4)) (addSug (numSug 3) (numSug 4)))
(test (parseSug '(+ 4 5)) (addSug (numSug 4) (numSug 5)))
(test (parseSug '(+ 4 6)) (addSug (numSug 4) (numSug 6)))
(test (parseSug '(+ 12 5)) (addSug (numSug 12) (numSug 5)))
(test (parseSug '(+ 26 3)) (addSug (numSug 26) (numSug 3)))
(test (parseSug '(- 4 3)) (subSug (numSug 4) (numSug 3)))
(test (parseSug '(- 42 5)) (subSug (numSug 42) (numSug 5)))
(test (parseSug '(- 4 6)) (subSug (numSug 4) (numSug 6)))
(test (parseSug '(- 12 5)) (subSug (numSug 12) (numSug 5)))
(test (parseSug '(- 26 3)) (subSug (numSug 26) (numSug 3)))
(test (parseSug '(- 3)) (negSug (numSug 3)))
(test (parseSug '(- 4)) (negSug (numSug 4)))
(test (parseSug '(- -4)) (negSug (numSug -4)))
(test (parseSug '(- 12)) (negSug (numSug 12)))
(test (parseSug '(- 26)) (negSug (numSug 26)))
(test (parseSug '(* 3 4)) (mulSug (numSug 3) (numSug 4)))
(test (parseSug '(* 4 5)) (mulSug (numSug 4) (numSug 5)))
(test (parseSug '(* 4 6)) (mulSug (numSug 4) (numSug 6)))
(test (parseSug '(* 12 5)) (mulSug (numSug 12) (numSug 5)))
(test (parseSug '(* 26 3)) (mulSug (numSug 26) (numSug 3)))
(test (parseSug '(** 3 4)) (powSug (numSug 3) (numSug 4)))
(test (parseSug '(** 4 5)) (powSug (numSug 4) (numSug 5)))
(test (parseSug '(** 4 6)) (powSug (numSug 4) (numSug 6)))
(test (parseSug '(** 12 5)) (powSug (numSug 12) (numSug 5)))
(test (parseSug '(** 26 3)) (powSug (numSug 26) (numSug 3)))
(test (parseSug '(+ 5 (* 6 3))) (addSug (numSug 5) (mulSug (numSug 6) (numSug 3))))
(test (parseSug '(+ 25 (- (** 2 4)))) (addSug (numSug 25) (negSug (powSug (numSug 2) (numSug 4)))))
(test (parseSug '(* 8 (+ 6 (+ 5 (- 3))))) (mulSug (numSug 8) (addSug (numSug 6) (addSug (numSug 5) (negSug (numSug 3))))))
(test (parseSug '(* 0 (** -5 3))) (mulSug (numSug 0) (powSug (numSug -5) (numSug 3))))
(test (parseSug '(+ (- 6 (- 1)) 8)) (addSug (subSug (numSug 6) (negSug (numSug 1))) (numSug 8)))

;; eval : myNum -> number
;; Evaluates a myNum and returns the result as a number.
;; Examples
;; (num 4) -> 4
;; (num 7) -> 7
;; (num 12) -> 12
;; (num -12) -> -12
;; (num 25) -> 25
;; (add (num 5) (num 4)) -> 9
;; (add (num 10) (num 5)) -> 15
;; (add (num 1) (num -4)) -> -3
;; (add (num 2) (num 6)) -> 8
;; (add (num 0) (num 0)) -> 0
;; (neg (num 5)) -> -5
;; (neg (num 10)) -> -10
;; (neg (num 0)) -> 0
;; (neg (num -2)) -> 2
;; (neg (num -124)) -> 124
;; (mul (num 5) (num 4)) -> 20
;; (mul (num 11) (num 5)) -> 55
;; (mul (num 1) (num -4)) -> -4
;; (mul (num -2) (num -6)) -> 12
;; (mul (num 0) (num 8)) -> 0
;; (pow (num 5) (num 3)) -> 125
;; (pow (num 1) (num 5)) -> 1
;; (pow (num -3) (num 3)) -> -27
;; (pow (num -2) (num 6)) -> 64
;; (pow (num 0) (num 5)) -> 0
;; (add (add (num 3) (num 4)) (num 35)) -> 42
;; Template
;; (define (eval [expr : myNum])
;;   (type-case myNum expr
;;    [num (n) ...(n)]
;;    [add (lhs rhs) (+ ...(lhs) ...(rhs))]
;;    [neg (n) ...(n)]
;;    [mul (lhs rhs) (* ...(lhs) ...(rhs)]
;;    [pow (lhs rhs) (takePower ...(lhs) ...(rhs))]))
(define (eval [expr : myNum])
  (type-case myNum expr
    [num (n) n]
    [add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [neg (n) (* (eval n) -1)]
    [mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [pow (lhs rhs) (takePower (eval lhs) (eval rhs))]))

(test (eval (num 4))  4)
(test (eval (num 7))  7)
(test (eval (num 12))  12)
(test (eval (num -12))  -12)
(test (eval (num 25))  25)
(test (eval (add (num 5) (num 4))) 9)
(test (eval (add (num 10) (num 5))) 15)
(test (eval (add (num 1) (num -4))) -3)
(test (eval (add (num 2) (num 6))) 8)
(test (eval (add (num 0) (num 0))) 0)
(test (eval (neg (num 5))) -5)
(test (eval (neg (num 10))) -10)
(test (eval (neg (num 0))) 0)
(test (eval (neg (num -2))) 2)
(test (eval (neg (num -124))) 124)
(test (eval (mul (num 5) (num 4))) 20)
(test (eval (mul (num 11) (num 5))) 55)
(test (eval (mul (num 1) (num -4))) -4)
(test (eval (mul (num -2) (num -6))) 12)
(test (eval (mul (num 0) (num 8))) 0)
(test (eval (pow (num 5) (num 3))) 125)
(test (eval (pow (num 1) (num 5))) 1)
(test (eval (pow (num -3) (num 3))) -27)
(test (eval (pow (num -2) (num 6))) 64)
(test (eval (pow (num 0) (num 5))) 0)
(test (eval (add (add (num 3) (num 4)) (num 35)))  42)

;; evalSug : myNumSug -> number
;; Evaluates a myNumSug and returns the result as a number.
;; Examples
;; (numSug 4) -> 4
;; (numSug 7) -> 7
;; (numSug 12) -> 12
;; (numSug -12) -> -12
;; (numSug 25) -> 25
;; (addSug (numSug 5) (numSug 4)) -> 9
;; (addSug (numSug 10) (numSug 5)) -> 15
;; (addSug (numSug 1) (numSug -4)) -> -3
;; (addSug (numSug 2) (numSug 6)) -> 8
;; (addSug (numSug 0) (numSug 0)) -> 0
;; (subSug (numSug 5) (numSug 4)) -> 1
;; (subSug (numSug 10) (numSug 5)) -> 5
;; (subSug (numSug 1) (numSug -4)) -> -5
;; (subSug (numSug 2) (numSug 6)) -> -4
;; (subSug (numSug 0) (numSug 0)) -> 0
;; (negSug (numSug 5)) -> -5
;; (negSug (numSug 10)) -> -10
;; (negSug (numSug 0)) -> 0
;; (negSug (numSug -2)) -> 2
;; (negSug (numSug -124)) -> 124
;; (mulSug (numSug 5) (numSug 4)) -> 20
;; (mulSug (numSug 11) (numSug 5)) -> 55
;; (mulSug (numSug 1) (numSug -4)) -> -4
;; (mulSug (numSug -2) (numSug -6)) -> 12
;; (mulSug (numSug 0) (numSug 8)) -> 0
;; (powSug (numSug 5) (numSug 3)) -> 125
;; (powSug (numSug 1) (numSug 5)) -> 1
;; (powSug (numSug -3) (numSug 3)) -> -27
;; (powSug (numSug -2) (numSug 6)) -> 64
;; (powSug (numSug 0) (numSug 5)) -> 0
;; (addSug (addSug (numSug 3) (numSug 4)) (numSug 35)) -> 42
(define (evalSug [expr : myNumSug])
  (eval (desugar expr)))

(test (evalSug (numSug 4))  4)
(test (evalSug (numSug 7))  7)
(test (evalSug (numSug 12))  12)
(test (evalSug (numSug -12))  -12)
(test (evalSug (numSug 25))  25)
(test (evalSug (addSug (numSug 5) (numSug 4))) 9)
(test (evalSug (addSug (numSug 10) (numSug 5))) 15)
(test (evalSug (addSug (numSug 1) (numSug -4))) -3)
(test (evalSug (addSug (numSug 2) (numSug 6))) 8)
(test (evalSug (addSug (numSug 0) (numSug 0))) 0)
(test (evalSug (subSug (numSug 5) (numSug 4))) 1)
(test (evalSug (subSug (numSug 10) (numSug 5))) 5)
(test (evalSug (subSug (numSug 1) (numSug -4))) 5)
(test (evalSug (subSug (numSug 2) (numSug 6))) -4)
(test (evalSug (subSug (numSug 0) (numSug 0))) 0)
(test (evalSug (negSug (numSug 5))) -5)
(test (evalSug (negSug (numSug 10))) -10)
(test (evalSug (negSug (numSug 0))) 0)
(test (evalSug (negSug (numSug -2))) 2)
(test (evalSug (negSug (numSug -124))) 124)
(test (evalSug (mulSug (numSug 5) (numSug 4))) 20)
(test (evalSug (mulSug (numSug 11) (numSug 5))) 55)
(test (evalSug (mulSug (numSug 1) (numSug -4))) -4)
(test (evalSug (mulSug (numSug -2) (numSug -6))) 12)
(test (evalSug (mulSug (numSug 0) (numSug 8))) 0)
(test (evalSug (powSug (numSug 5) (numSug 3))) 125)
(test (evalSug (powSug (numSug 1) (numSug 5))) 1)
(test (evalSug (powSug (numSug -3) (numSug 3))) -27)
(test (evalSug (powSug (numSug -2) (numSug 6))) 64)
(test (evalSug (powSug (numSug 0) (numSug 5))) 0)
(test (evalSug (addSug (addSug (numSug 3) (numSug 4)) (numSug 35)))  42)

;; evalExpression : s-expression -> number
;; Parses and evaluates s-expression
;; Examples
;; '(+ 5 3) -> 8
;; '(+ 3 4) -> 7
;; '(+ -3 4) -> 1
;; '(+ 13 6) -> 19
;; '(+ -4 -5) -> -9
;; '(- 5 3) -> 2
;; '(- 3 4) -> -1
;; '(- -3 4) -> -7
;; '(- 13 6) -> 7
;; '(- -4 -5) -> 1
;; '(- 5) -> -5
;; '(- 12) -> -12
;; '(- 0) -> 0
;; '(- -5) -> 5
;; '(- -12) -> 12
;; '(* 5 3) -> 15
;; '(* 3 4) -> 12
;; '(* -3 4) -> -12
;; '(* 3 6) -> 18
;; '(* -4 -5) -> 20
;; '(** 5 3) -> 125
;; '(** 3 3) -> 27
;; '(** -3 3) -> -27
;; '(** -6 2) -> 36
;; '(** 0 5) -> 0
;; '(+ 5 (- (* 4 2) (- 3))) -> 16
;; '(* (** (- 5) 2) (- (+ 1 2))) -> -75
;; '(* 123 (- (- 5 (- -5)))) -> 0
;; '(+ (** -5 (- -2)) (- 24)) -> 1
;; '(- (** 2 5) (+ 4 (** (* 3 (- 5 (- 6))) 1))) -> -5
;; Template
;; (define (evalExpression [expr : s-expression]) : number
;;   (evalSug ...
;;    (parseSug ...)))
(define (evalExpression [expr : s-expression]) : number
  (evalSug (parseSug expr)))

(test (evalExpression '(+ 5 3)) 8)
(test (evalExpression '(+ 3 4)) 7)
(test (evalExpression '(+ -3 4)) 1)
(test (evalExpression '(+ 13 6)) 19)
(test (evalExpression '(+ -4 -5)) -9)
(test (evalExpression '(- 5 3)) 2)
(test (evalExpression '(- 3 4)) -1)
(test (evalExpression '(- -3 4)) -7)
(test (evalExpression '(- 13 6)) 7)
(test (evalExpression '(- -4 -5)) 1)
(test (evalExpression '(- 5)) -5)
(test (evalExpression '(- 12)) -12)
(test (evalExpression '(- 0)) 0)
(test (evalExpression '(- -5)) 5)
(test (evalExpression '(- -12)) 12)
(test (evalExpression '(* 5 3)) 15)
(test (evalExpression '(* 3 4)) 12)
(test (evalExpression '(* -3 4)) -12)
(test (evalExpression '(* 3 6)) 18)
(test (evalExpression '(* -4 -5)) 20)
(test (evalExpression '(** 5 3)) 125)
(test (evalExpression '(** 3 3)) 27)
(test (evalExpression '(** -3 3)) -27)
(test (evalExpression '(** -6 2)) 36)
(test (evalExpression '(** 0 5)) 0)
(test (evalExpression '(+ 5 (- (* 4 2) (- 3)))) 16)
(test (evalExpression '(* (** (- 5) 2) (- (+ 1 2)))) -75)
(test (evalExpression '(* 123 (- (- 5 (- -5))))) 0)
(test (evalExpression '(+ (** -5 (- -2)) (- 24))) 1)
(test (evalExpression '(- (** 2 5) (+ 4 (** (* 3 (- 5 (- 6))) 1)))) -5)