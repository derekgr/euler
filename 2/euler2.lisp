(defun fib-evens (iteration n n1 acc)
  (cond
    ((> n 1000000)
     (list n acc))
    (t
     (let 
       ((this-value (+ n n1)))
       (fib-evens (+ iteration 1) this-value n (if (eql (mod iteration 3) 2) (+ acc this-value) acc))))))

(defun euler2 ()
  (second (fib-evens 1 1 0 0)))
