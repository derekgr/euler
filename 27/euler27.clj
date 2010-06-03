(ns euler27) 

(set! *warn-on-reflection* true) 

;; sieve of erasthothenes.
(defn primes
  ([] (cons 1 (primes (drop 2 (range)) (sorted-set))))
  ([[h & t] acc]
    (if (some #(zero? (mod h %)) acc) (primes t acc)
      (lazy-seq (cons h (primes t (conj acc h)))))))

(defn prime-set [n] (into (sorted-set) (take n (primes))))

(defn quad [a b n]
  (+ (* n n) (* a n) b))

(defn quad-seq [a b]
  (map #(quad a b %) (range)))

(let [pset (prime-set 1000)] 
  (defn quad-length [a b]
    (let [stream (quad-seq a b)]
      (count (take-while #(and (pos? %) (pset %)) stream)))))

(let [pset (take-while #(< % 1000) (primes))]
  (defn all-quads []
    (for [x (range -999 1000) y pset] [(quad-length x y) x y]))
)

(defn euler27 []
  (reduce (fn [a b] (if (> (b 0) (a 0)) b a)) [0 nil] (all-quads)))
