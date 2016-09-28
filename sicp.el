;; Exercises from SICP in elisp

;; 1.1.1 Expressions

486
486

(+ 137 349)
486

(- 1000 334)
666

(* 5 99)
495

(/ 10 5)
2

(+ 2.7 10)
12.7

(+ 21 35 12 7)
75

(* 25 4 12)
1200

(+ (* 3 5) (- 10 6))
19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
57

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
57

;; 1.1.2 Naming and the Environment

(defvar size 2)
size

size
2

(* 5 size)
10

(defvar pi 3.14159)
pi

(defvar radius 10)
radius

(* pi (* radius radius))
314.1592653589793

(defvar circumference (* 2 pi radius))
circumference

circumference
62.83185307179586

;; 1.1.3 Evaluating Combinations

(* (+ 2 (* 4 6))
   (+ 3 5 7))
390

;; 1.1.4 Compound Procedures

(defun square (x) (* x x))
square

(square 21)
441

(square (+ 2 5))
49

(square (square 3))
81

(defun sum-of-squares (x y)
  (+ (square x) (square y)))
sum-of-squares

(sum-of-squares 3 4)
25

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))
f

(f 5)
136

;; 1.1.5 The Substitution Model for Procedure Application

;; applicative-order evaluation
;; aka "evaluate args then apply"
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

;; normal-order evaluation
;; aka "fully expand and then reduce"
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

;; 1.1.6 Conditional Expressions and Predicates

(defun abs (x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(defun abs (x)
  (cond ((< x 0) (- x))
        (t x)))

(defun abs (x)
  (if (< x 0)
      (- x)
    x))

(defun >= (x y)
  (or (> x y) (= x y)))

(defun >= (x y)
  (not (< x y)))

;; Exercise 1.1

10
10

(+ 5 3 4)
12

(- 9 1)
8

(/ 6 2)
3

(+ (* 2 4) (- 4 6))
6

(defvar a 3)
a

(defvar b (+ a 1))
b

(+ a b (* a b))
19

(= a b)
nil

(if (and (> b a) (< b (* a b)))
    b
  a)
4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (t 25))
16

(+ 2 (if (> b a) b a))
6

(* (cond ((> b a) a)
         ((< a b) b)
         (t -1))
   (+ a 1))
12

;; Exercise 1.2

(/ (+ 5 (/ 1 5) (- 2 (- 3 (+ 6 (/ 1 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3

(defun sum-of-square-of-largest-two (x y z)
  (cond ((= x (min x y z)) (sum-of-squares y z))
        ((= y (min x y z)) (sum-of-squares x z))
        ((= z (min x y z)) (sum-of-squares x y))))

(sum-of-square-of-largest-two 1 2 3)
13

;; Exercise 1.4

(defun a-plus-abs-b (a b)
  "Return a plus the absolute value of b"
  (if (> b 0)
      (+ a b)
    (- a b)))
(a-plus-abs-b 10 -5)
15

;; Exercise 1.5

(defun p nil (p))

(defun test (x y)
  (if (= x 0)
      0
    y))

(test 0 (p))

;; 1.1.7 Example: Square Roots by Newton's Method

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
    (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt (x)
  (sqrt-iter 1.0 x))

(sqrt 9)
3.00009155413138

(sqrt (+ 100 37))
11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
1.7739279023207892

(square (sqrt 1000))
1000.000369924366

;; Exercise 1.6
;; ordering => else always evaluates => infinite loop

;; Exercise 1.7

(defun good-enough? (guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

(sqrt-iter 1.0 9)
3.00009155413138

(sqrt-iter 1.0 0.001)
0.031642015868650786

;; Exercise 1.8

(defun cube-root (guess x)
  (if (good-enough-yet? guess x)
      guess
    (cube-root (improve-cube-root-guess guess x) x)))

(defun improve-cube-root-guess (guess x)
  (/
   (+
    (/ x (* guess guess))
    (* 2 guess))
   3.0))

(defun good-enough-yet? (guess x)
  (<
   (abs (- (* guess guess guess) x))
   0.01))

(cube-root 1.0 9)
