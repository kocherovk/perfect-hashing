;; This section of the code contains functions
;; that is essential to the mapping step of the algorythm.

(ns perfect_hashing.mapping)

(defn random-table [N max-length]
  (let [table (object-array 256)]
    (doseq [i (range 256)]
      (aset table i (object-array max-length))
      (doseq [j (range max-length)]
        (aset (aget table i) j (rand-int N))))
    table))

(defn with-table [N max-length]
  (let [table (random-table N max-length)]
    (fn [word]
      (loop [sum 0 i (range (count word)) c (seq word)]
        (if (empty? c)
          sum
          (recur (rem (+ sum (aget (aget table (int (first c))) (inc (first i)))) N)
                 (rest i)
                 (rest c)))))))


;; Old and quite incorrect functions below
;; #_ means than reader skips next statement
#_
(defn random-table [N]
  "Function generates random map with keys [a-z, A-Z, _, 0-9]
  and values < N"
  (let [fill-table (fn [nums m]
                     (if (empty? nums)
                       m
                       (recur (rest nums) (assoc m (char (first nums)) (rand-int  N)))))]
  (fill-table (range 256) {})))

#_
(defn with-table [N]
  "Returns a function that calculates an integer
   with given string using random-table"
  (let [table (random-table N)
        base 67]
    (fn [word]
      (loop [sum 0 a (seq word) P 1]
        (if (empty? a)
          sum
          (recur (rem (+ sum (* P (table (first a)))) N)
                 (rest a)
                 (* P base)))))))
