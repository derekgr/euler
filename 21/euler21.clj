(ns euler21)

(defn divisors [n]
  (let [limit (Math/sqrt n)]
    (letfn [(next-divisor [i] 
                          (if (> i limit) nil
                            (if (zero? (mod n i)) 
                              (lazy-seq (cons i (cons (/ n i) (next-divisor (inc i)))))
                              (next-divisor (inc i)))))]
    (cons 1 (next-divisor 2)))))

(defn d [n]
  (reduce + (divisors n)))

(let
  [memo-d (memoize d)]
  (defn amicable? [a]
    (let [b (memo-d a)
          maybe_a (memo-d b)]
      (and (not (= a b)) (= a maybe_a)))))
          
(defn amicable-seq [limit]
  (filter amicable? (range 2 limit)))

(defn euler21 []
  (reduce + (amicable-seq 10000)))
