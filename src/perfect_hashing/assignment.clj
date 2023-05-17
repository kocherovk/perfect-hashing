(ns perfect_hashing.assignment
  (:use perfect_hashing.graph
        perfect_hashing.util
        perfect_hashing.disjoint_set
        perfect_hashing.mapping))

(defn traverce [graph]
  "Function, that computes function g"
  (let [N (graph :n)
        M (graph :m)
        visited (boolean-array (inc N))
        g (int-array (inc N))
        go (fn go[v]
             (aset visited v true)
             (doseq [e (adjacent-edges graph v)]
               (when-not (aget visited ((graph :node) :get e))
                 (aset g ((graph :node) :get e) (mod+ (- (abs e) 1 (aget g v)) M))
                 (go ((graph :node) :get e)))))]
    (doseq [v (range 1 (inc N))]
      (if (not (aget visited v))
        (go v)))
    g))

(defn make-perfect [g fn1 fn2 M]
  "Creates perfect hash function by composing g function"
  (when (> M 0)
    (fn [word]
      (when word
        (rem (+ (aget g (fn1 word)) (aget g (fn2 word))) M)))))

(def WORD-MAX-LENGTH 20)

(defn gimme-perfect
  "Main function for generating perfect hash function"
  ([words] (gimme-perfect words nil))
  ([words mods] (gimme-perfect words 3 mods))
  ([words c mods]
   (let [debug (has? mods :debug)
         M (count words)
         N (* M c)

         try-construct (fn[fn1 fn2]
                         (let [dset (make-disjoint-set (inc N))
                               NODE (make-mid0array M)
                               NEXT (make-mid0array M)
                               FIRST (int-array (inc N))
                               edges (calculate-edges words fn1 fn2)

                               add-edge (fn [e i]
                                          (if (find? dset (from e) (to e))
                                            false
                                            (do
                                              (union dset (from e) (to e))
                                              (NODE :set (- i) (from e))
                                              (NODE :set (+ i) (to e))
                                              true)))

                              add-edges (fn [edges]
                                          (let [processed (atom 0)]
                                            (loop [edg edges i 1]
                                              (when (and debug (>= (/ (- i @processed) M) 0.05))
                                                (print "I")
                                                (reset! processed i))
                                              (cond
                                               (empty? edg) true
                                               (add-edge (first edg) i) (recur (rest edg) (inc i))
                                               :else (when debug (println " failed!"))))))

                               init (fn []
                                      (doseq [e (reverse (range 1 (inc M)))]
                                        (NEXT :set e (aget FIRST (NODE :get (- e))))
                                        (aset FIRST (NODE :get (- e)) e)
                                        (NEXT :set (- e) (aget FIRST (NODE :get e)))
                                        (aset FIRST (NODE :get e) (- e))))]

                           (if (add-edges edges)
                             (do
                               (init)
                               {:node NODE :next NEXT :first FIRST :n N :m M})
                             nil)))

         try-build (fn[i]
                     (let [fn1 (with-table N WORD-MAX-LENGTH)
                           fn2 (with-table N WORD-MAX-LENGTH)
                           graph (try-construct fn1 fn2)]
                       (if (nil? graph)
                         (recur (inc i))
                         (do
                           (when debug (println "\nDone in" i "iterations"))
                           (make-perfect (traverce graph) fn1 fn2 M)))))]
     (if debug
       (time (try-build 1))
       (try-build 1)))))
