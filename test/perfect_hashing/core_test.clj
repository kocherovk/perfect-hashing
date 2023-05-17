(ns perfect_hashing.core_test
  (:use clojure.test
        perfect_hashing.core
        perfect_hashing.input
        perfect_hashing.graph
        perfect_hashing.mapping
        perfect_hashing.assignment
        perfect_hashing.defs)
  (:import java.util.Date))

(defn now [] (. System currentTimeMillis))

(defn time-consumed [expr]
  (let [t (now)]
    (eval expr)
    (- (now)  t)))

(defn avr-time [expr N]
  (loop [sum 0 i 0]
    (if (= i N)
      (float (/ sum N))
      (recur (+ sum (time-consumed expr)) (inc i)))))

(defn get-test [N]
  `(gimme-perfect (take ~N (input-from-file "test//dict.txt"))[:debug]))

(avr-time (get-test 120000) 5)
