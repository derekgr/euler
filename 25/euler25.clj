(ns euler25) 

(defn digits [n]
  (loop [i n
         acc 0]
    (if (< i 10) (inc acc) (recur (quot i 10) (inc acc))))) 

(defn fib [n]
  (loop [i 1
         j 0
         cnt 1]
    (if (>= cnt n) i
      (recur (+ i j) i (inc cnt)))))

(defn fibseq [n]
  (map #(list % (fib %)) (range 1 n))) 

(defn euler25 []
  (some (fn [[i n]] (if (= (digits n) 1000) i)) (fibseq Double/POSITIVE_INFINITY)))
