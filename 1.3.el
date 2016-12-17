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

(sum-cubes 1 10)
3025

(defun identity (x) x)

(defun sum-integers (a b)
  (sum 'identity a 'inc b))

(sum-integers 1 10)
55

(defun pi-sum (a b)
  (defun pi-term (x)
    (/ 1.0 (* x (+ x 2))))
  (defun pi-next (x)
    (+ x 4))
  (sum 'pi-term a 'pi-next b))

(* 8 (pi-sum 1 1000))
3.139592655589783

(defun integral (f a b dx)
  (defun add-dx (x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) 'add-dx b)
     dx))

(integral 'cube 0 1 0.01)
0.24998750000000042
(integral 'cube 0 1 0.001)
