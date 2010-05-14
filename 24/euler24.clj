(ns euler24)

(defn permute [coll]
"Returns a lazy sequence of the permutations of coll as cons cells;
each car is the item in the permutation, and each cdr is a sequence
of the tails of the permutations starting with that element. If coll is in
lexicographical order, so are the permutations produced by walking the result of
permute depth-first."
  (when-let [s (seq coll)]
    (map (fn [x] (cons x (permute (remove #(= x %) s)))) s)))

(defn factorial [n]
"Returns n! for positive integer n."
  (loop [cnt n acc 1]
    (if (<= cnt 0)
      acc
      (recur (dec cnt) (* acc cnt)))))

(defn- seek
  ([s parts i acc]
  "Seeks to the permutation at index i in permutation sequence s, representing partitions of
parts remaining elements. Accumulates sequence values in acc until desired index is reached."
   (if (empty? s) acc
     (let [partsize (factorial (dec parts))
           permutation (nth s (quot i partsize))
           data (first permutation)
           remainder (rest permutation)]
       (recur remainder (dec parts) (mod i partsize) (conj acc data)))))
  ([s parts i] (seek s parts i [])))

(defn ith-permutation [n i]
  "Generate the ith lexicographical permutation of the numbers from 0..n (inclusive)."
  (let [coll (range (inc n))]
    (seek (permute coll) (count coll) (dec i))))

(println (ith-permutation 9 1000000))
