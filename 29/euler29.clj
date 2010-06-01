(ns euler29) 

(defn ranges [n]
  (for [a (range 2 (inc n)) b (range 2 (inc n))] (Math/pow a b))) 

(defn terms [n]
  (count (into #{} (ranges n))))

(defn euler29 [] (terms 100))
