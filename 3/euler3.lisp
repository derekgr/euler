(defun euler-list ()
  (let
     ((the-list))
     (do ((x 563545 (- x 2)))
       ((<= x 1) the-list)
      (setq the-list (if (eql (gcd x 13195) 1) (cons x the-list) the-list)))))

(defun euler3 ()
  (dolist (x (reverse (euler-list)))
    (if (zerop (mod 317584931803 x)) (return x))))
