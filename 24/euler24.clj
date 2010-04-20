(ns euler.problem24)

(defn permute [coll]
  "Returns a lazy sequence of cons cells. Each cell's car is the head of the list of the 
  permutation it represents. Each cell's cdr is another lazy sequence representing possible
  permutations with that prefix, and so on."
  (when-let [s (seq coll)]
    ;; each element of coll will become the head of a new subsequence.
    (map (fn [x] (cons x (permute (filter #(not= x %) s)))) s)))

(defn factorial [n]
  (loop [cnt n acc 1]
    (if (<= cnt 0)
      acc
      (recur (dec cnt) (* acc cnt)))))

(defn- div [a b]
  (int (/ a b)))

(defn- branch [s parts i acc]
  "Seeks to the permutation at index i in permutation sequence s, representing partitions of 
  parts remaining elements. Accumulates sequence values in acc until desired index is reached."
  (let [partsize (factorial (dec parts))]
    (cond
      (< i partsize) 
      (let [permutation (first s)
            data (first permutation)
            remainder (rest permutation)]
        (if (nil? data) acc
          (recur remainder (dec parts) i (conj acc data))))
      :else
        (let [permutation (nth s (div i partsize))
              data (first permutation)
              remainder (rest permutation)]
          (recur remainder (dec parts) (mod i partsize) (conj acc data))))))

(defn nth-permutation [coll n]
  "Generate the nth lexicographical permutation of the items in collection coll, assuming
  items given occur in lexicographically increasing order in the collection."
  (branch (permute coll) (count coll) (dec n) []))

