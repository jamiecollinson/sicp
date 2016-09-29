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
