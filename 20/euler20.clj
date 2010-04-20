(ns euler.problem20)

(defn digitize [n]
  (if (zero? n) nil (lazy-seq (cons (mod n 10) (digitize (quot n 10))))))
