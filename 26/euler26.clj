(ns euler26) 

(set! *warn-on-reflection* true)

(def *bufsize* 10000)

(defn decseq [a b]
  (if
    (= 0 a) nil 
    (let [n (* 10 a)
          q (quot n b)
          r (rem n b)]
      (lazy-seq (cons (int q) (decseq (int r) (int b)))))))

(defn unit [n] (decseq 1 n)) 

(defn strexpand 
  ([s] (strexpand s (StringBuilder. (int (inc *bufsize*)))))
  ([s #^StringBuilder buf]
   (when (seq s)
    (do
      (.append buf (int (first s))) 
      (lazy-seq (cons (.toString buf) (strexpand (rest s) buf)))))))

(let [re #"((\d{200,})\2){3,}$"]
  (defn length-from [strs acc]
    (if (seq strs) 
      (if (< (.length #^String (first strs)) *bufsize*)
        (if-let [m (re-matches re (first strs))] 
          [(.length #^String (nth m 2)) (nth m 2)]
          (recur (rest strs) (inc acc)))
        [-1 nil])
      [acc nil])))

(defn length [n] (length-from (strexpand (unit n)) 0))

(defn length-seq [n] 
  (map (fn [i] [i (length i)]) (reverse (range 10 n))))

(defn repeating-cycles [n]
  (for [[i [l rpt :as s]] (length-seq n) :when (not (nil? rpt))] [i l rpt]))

(defn euler26 [] 
  (let [cycles (repeating-cycles 1000)]
    (some (fn [[i l _ :as a]] (if (= i (inc l)) a)) cycles)))
