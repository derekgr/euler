(ns euler28) 

(defn msqr [n]
  (let [t (inc (* 2 (dec n)))]
    (* t t)))

(defn diag [n]
  (if (<= n 1) (cons 1 nil)
    (let [sofar (diag (dec n))
          step (* 2 (dec n))
          start (+ (msqr (dec n)) step)]
      (reduce (fn [l i] (cons i l)) sofar (range (+ start) (+ (* 4 step) start) step)))))

(defn sum [length]
  (reduce + (diag (int (inc (/ length 2))))))

(defn euler27 [] (sum 1001))
