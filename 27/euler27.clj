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
      (loop [[h & t] stream i 0]
        (if (and (pos? h) (contains? pset h)) (recur t (inc i)) i)))))

(defn all-quads []
  (for [x (range -999 1000) y (range -999 1000)] [(quad-length x y) [x y]]))

(defn euler27 []
  (reduce (fn [[l1 _ :as a] [l2 _ :as b]] (if (> l2 l1) b a)) [0 nil] (all-quads)))
