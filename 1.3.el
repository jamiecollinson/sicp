;; 1.3.1 Procedures as Arguments

(defun sum-integers (a b)
  (if (> a b)
      0
    (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 3)
6

(defun sum-cubes (a b)
  (defun cube (a) (* a a a))
  (if (> a b)
      0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(sum-cubes 1 3)
36

(defun pi-sum (a b)
  (if (> a b)
      0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(pi-sum 1 3)
0.3333333333333333

(defun sum (term a next b)
  (if (> a b)
      0
    (+ (funcall term a)
       (sum term (funcall next a) next b))))

(defun inc (n) (+ n 1))
(defun cube (n) (* n n n))
(defun sum-cubes (a b)
  (sum 'cube a 'inc b))

(sum-cubes 1 3)
36
