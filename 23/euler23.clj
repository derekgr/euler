(ns euler23) 

(defn divsum [n]
  (loop [i 2
         upper (int (Math/sqrt n)) 
         acc 1]
    (cond
      (>= i upper) acc
      (zero? (mod n i)) 
        (let [q (int (/ n i))
              newacc (+ acc i (if (= q i) 0 q))]
        (recur (inc i) q newacc)) 
      :else (recur (inc i) upper acc))))

(def abundant? 
  (memoize (fn [n] (> (divsum n) n))))

(defn summors [n]
  (for [i (range 2 (inc (int (/ n 2))))] [i (- n i)])) 

(defn good? [n]
  (some (fn [[a b :as c]] (if (and (abundant? a) (abundant? b)) c)) (summors n)))

(defn euler23 []
  (reduce + (remove good? (range 1 28124)))) 
