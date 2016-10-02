;; Exercises from SICP in elisp

;; 1.2 Procedures and the Processes They Generate

;; 1.2.1 Linear Recursion and Iteration

(defun factorial (n)
  (if (= n 1)
      1
    (* n (factorial (- n 1)))))

(factorial 6)
720

(defun factorial (n)
  (defun fact-iter (product counter max-count)
    (if (> counter max-count)
        product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
  (fact-iter 1 1 n))

(factorial 6)
720

;; Exercise 1.9

(defun + (a b)
  (if (= a 0)
      b
    (1+ (+ (1- a) b))))

(+ 4 5)
(1+ (+ 3 5))
(1+ (1+ (+ 2 5)))
(1+ (1+ (1+ (+ 1 5))))
(1+ (1+ (1+ (1+ (+ 0 5)))))
(1+ (1+ (1+ (1+ 5))))
(1+ (1+ (1+ 6)))
(1+ (1+ 7))
(1+ 8)
9

;; => linear recursive process

(defun + (a b)
  (if (= a 0)
      b
    (+ (1- a) (1+ b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;; => linear iterative process

;; Exercise 1.10

;; Ackermann's function
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1)
              (A x (- y 1))))))

(A 1 10)
1024
(A 2 4)
65536
(A 3 3)
65536

(defun f (n) (A 0 n))
;; => f(n) = 2*n

(defun g (n) (A 1 n))
;; => g(n) = 2^n

(defun h (n) (A 2 n))
;; => h(n) = ?

;; 1.2.2 Tree Recursion

(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(fib 5)
5

(defun fib (n)
  (defun fib-iter (a b count)
    (if (= count 0)
        b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 5)
5

;; Example: Counting change

(defun count-change (amount)
  (defun first-denomination (kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (defun cc (amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (t (+ (cc amount
                    (- kinds-of-coins 1))
                (cc (- amount
                       (first-denomination kinds-of-coins))
                    kinds-of-coins)))))
  (cc amount 5))

(count-change 100)
292

;; Exercise 1.11

(defun f-recursive (n)
  (if (< n 3) n
    (+ (f-recursive (- n 1))
       (* 2 (f-recursive (- n 2)))
       (* 3 (f-recursive (- n 3))))))

(mapcar 'f-recursive (number-sequence 1 20))
(1 2 4 11 25 59 142 335 796 1892 4489 10661 ...)

(defun f-iterative (n)
  (defun f-iter (a b c count)
    (if (= count n)
        a
      (f-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (+ count 1))))
  (if (< n 3)
      n
    (f-iter 2 1 0 2)))

(mapcar 'f-iterative (number-sequence 1 20))
(1 2 4 11 25 59 142 335 796 1892 4489 10661 ...)

;; Exercise 1.12

(defun pascal (x y)
  "Compute the number at row x and column y of pascal's triangle"
  (cond ((= y 1) 1)
        ((= y x) 1)
        (t (+ (pascal (- x 1) (- y 1))
              (pascal (- x 1) y)))))

(pascal 1 1)
1
(pascal 5 3)
6

;; Exercise 1.13

;; Prove Fib(n) is the closest integer to (phi^n)/sqrt(5) where phi = (1 + sqrt(5))/2. HINT: let psi = (1-sqrt(5))/2 Use induction to prove that Fib(n) = (phi^n - psi^n)/sqrt(5)

;; 1.2.3 Orders of Growth

;; Exercise 1.14
;; The count change procedure is O(n) (linear) in space
;; It is O(n^k) (= O(n^5)) in number of steps

;; Exercise 1.15

(defun sine (angle)
  (defun cube (x) (* x x x))
  (defun p (x) (- (* 3 x) (* 3 (cube x))))
  (if (< (abs angle) 0.1)
      angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)
-1.0294553341885244

;; 1.2.4 Exponentiation

(defun expt (b n)
  (if (= n 0)
      1
    (* b (expt b (- n 1)))))

;; => O(n) steps and O(n) space

(expt 10 2)
100

(defun expt (b n)
  (defun expt-iter (counter product)
    (if (= counter 0)
        product
      (expt-iter (- counter 1)
                 (* product b))))
  (expt-iter n 1))

;; => O(n) steps and O(1) space

(expt 10 2)
100

(defun fast-expt (b n)
  (defun square (n) (* n n))
  (defun even? (n)
    (= (% n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

;; => O(log n) steps

(fast-expt 10 2)
100

;; Exercise 1.16

(defun fast-expt (b n)
  (defun square (x) (* x x))
  (defun even? (x) (= (% x 2) 0))
  (defun fast-expt-iter (b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (t (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

(fast-expt 5 5)
3125

;; Exercise 1.17

(defun * (a b)
  (if (= b 0)
      0
    (+ a (* a (- b 1)))))

;; => O(b) steps

(* 2 4)
8

(defun *-fast (a b)
  (defun double (x) (* x 2))
  (defun halve (x) (/ x 2))
  (defun even? (x) (= (% x 2) 0))
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (*-fast (double a) (halve b)))
        (t (+ a (*-fast a (- b 1))))))

;; => linear recursive process of O(log b)

(*-fast 2 4)
8

;; Exercise 1.18
;; Given 'double and 'halve design a multiplication procedure which is O(log b) and iterative

(defun *-fast (a b)
  (defun double (x) (* x 2))
  (defun halve (x) (/ x 2))
  (defun even? (x) (= (% x 2) 0))
  (defun *-fast-iter (a b c)
    (cond ((= b 1) (+ a c))
          ((even? b) (*-fast-iter (double a) (halve b) c))
          (t (*-fast-iter a (- b 1) (+ a c)))))
  (*-fast-iter a b 0))

(*-fast 2 2)
4
(*-fast 10 100)
1000

;; Exercise 1.19

(defun fib (n)
  (defun fib-iter (a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     ?
                     ?
                     (/ count 2)))
          (t (fib-iter (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1)))))
  (fib-iter 1 0 0 1 n))
