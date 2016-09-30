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
