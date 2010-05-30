(ns euler19) 

(defn mkdate []
  [1 1 1900 1]) 

(defn leap? [year]
  (and (zero? (mod year 4)) 
       (or (> (mod year 100) 0) (zero? (mod year 400))))) 

(defn eom? [day month leap]
  (or
    (and leap (and (= 29 day) (= 2 month)))
    (and (not leap) (= 28 day) (= 2 month))
    (and (= 30 day) (or (= 9 month) (= 4 month) (= 6 month) (= 11 month))) 
    (= 31 day))) 

(defn nextn [i n]
  (inc (mod i n)))

(defn incd [[day month year dow]]
  (let [ndow (nextn dow 7)
        nyear (if (and (= 31 day) (= 12 month)) (inc year) year) 
        eom (eom? day month (leap? year)) 
        nmonth (if eom (nextn month 12) month) 
        nday (if eom 1 (inc day)) 
        ]
    [nday nmonth nyear ndow] 
   )) 

(defn count? [[day _ year dow]]
  (and (= 7 dow) (= 1 day) (>= year 1901))) 

(defn dates 
  ([[_ _ year _ :as dt]] (when (< year 2001) (let [ndt (incd dt)] (lazy-seq (cons ndt (dates ndt))))) )
  ([] (lazy-seq (cons (mkdate) (dates (mkdate))))))

(defn euler19 []
  (count (filter count? (dates)))) 
