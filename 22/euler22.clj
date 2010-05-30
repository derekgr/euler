(ns euler22) 

(defn load-sorted-file [filename]
  (let [contents (slurp filename)
        names (re-seq #"\"([^\"]+)\",?" contents)
        sorted-names (sort (map last names))]
    sorted-names))

(defn aval [s]
  (reduce + (map #(- (int %) 64) (seq s)))) 

(defn score-seq [names]
  (let [scored (map aval names)]
    (reduce (fn [[total i] v] [(+ total (* i v)) (inc i)]) [0 1] scored))) 

(defn euler22 [filename]
  (score-seq (load-sorted-file filename))) 
