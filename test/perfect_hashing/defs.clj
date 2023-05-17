(ns perfect_hashing.defs
  (:use perfect_hashing.disjoint_set
        perfect_hashing.graph
        perfect_hashing.util))

(def add-edge (fn [e i NODE dset]
                   (if (find? dset (from e) (to e))
                     (println i)
                     (do
                       (union dset (from e) (to e))
                       (NODE :set (- i) (from e))
                       (NODE :set (+ i) (to e))
                       true))))
(def add-edges (fn [edges NODE dset]
                    (loop [edg edges i 1]
                      (cond
                       (empty? edg) true
                       (add-edge (first edg) i NODE dset) (recur (rest edg) (inc i))
                       :else nil))))

(def init (fn [NEXT NODE FIRST M]
               (doseq [e (reverse (range 1 (inc M)))]
                 (NEXT :set e (aget FIRST (NODE :get (- e))))
                 (aset FIRST (NODE :get (- e)) e)
                 (NEXT :set (- e) (aget FIRST (NODE :get e)))
                 (aset FIRST (NODE :get e) (- e)))))

(def try-construct
  (fn[fn1 fn2 N M words]
    (let [dset (make-disjoint-set (inc N))
          NODE (make-mid0array M)
          NEXT (make-mid0array M)
          FIRST (int-array (inc N))
          edges (calculate-edges words fn1 fn2)]

      (if (add-edges edges NODE dset)
        (do
          (init NEXT NODE FIRST M)
          {:node NODE :next NEXT :first FIRST :n N :m M})
        nil))))
