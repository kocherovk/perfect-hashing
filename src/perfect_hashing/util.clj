(ns perfect_hashing.util)

(defn fill-array [array n]
  (doseq [i (range (alength array))]
    (aset array i n))
  array)

(defn make-mid0array [N]
  (let [array (int-array (+ 1 (* 2 N)))]
    (fn self[action & args]
      (cond
       (= action :get) (aget array (+ N  (first args)))
       (= action :set) (aset array (+ N  (first args)) (second args))
       (= action :seq) (for [i (range 0  (+ 1 (* 2 N)))]
                         (aget array i))
       (= action :print) (doseq [i (range (- N) N)]
                           (println (self :get i)))))))

(defn now [] (new java.util.Date))

(defn eval-time [expr]
  (let [time0 (now)]
    (eval expr)
    (- (. (now) getTime) (. time0 getTime))))

(defn avr-time [expr tests-count]
  (loop [i tests-count sum 0]
    (if (zero? i)
      (float (/ sum tests-count))
      (recur (dec i) (+ sum (eval-time expr))))))

(def != (complement =))

(defn correct? [perfect words]
  (loop [w words i (range (count w))]
    (cond
     (empty? w) true
     (!= (first i) (perfect (first w))) false
     :else (recur (rest w) (rest i)))))

(defn abs [x]
  (if (neg? x)
    (- x)
    x))

(defn mod+ [v m]
  (if (neg? v)
    (+ (rem v m) m)
    (rem v m)))

(defn has? [coll value]
  (some #(= value %) coll))
