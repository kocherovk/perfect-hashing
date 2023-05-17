;; This file contains definitions of edge and graph data structures
;; and various algorythms, which is working with them.

(ns perfect_hashing.graph
  (:use perfect_hashing.mapping
        perfect_hashing.util))

(defn new-edge [from to]
  "Construct new edge"
  (fn [n]
    (cond
     (= n 0) from
     (= n 1) to)))

(defn from [edge]
  "Return first component from edge"
  (edge 0))

(defn to [edge]
  "Returns second component from edge"
  (edge 1))

(defn make-graph [N M]
  "Create new graph"
  (let [node (int-array (inc M))
        next (fill-array (int-array (inc M)))
        first (int-array (inc N))]
  {:node node :next next :first first :n N :m M}))

(defn adjacent-edges [graph v]
  "Returns edges, adjacent to node v"
  (loop [edges '() e (aget (graph :first) v)]
    (if (zero? e)
      edges
      (recur (conj edges e) ((graph :next) :get e)))))

(defn adjacent-nodes [graph v]
  "Returns nodes, adjecent to node v"
  (loop [e (adjacent-edges graph v)
         nodes (hash-set)]
    (if (empty? e) (seq nodes)
      (recur (rest e) (conj nodes ((graph :node) :get (first e)))))))

 (defn print-graph [G]
   "Prints graph"
   (let [nodes (range 1 (G :n))
         edges (for [i nodes]
                 (filter #(pos? %) (adjacent-edges G i)))]
     (doseq [[nod edg] (map list nodes edges)]
       (print nod " -> ")
       (doseq [n edg]
         (print ((G :node) :get n) " "))
       (println))))

(defn calculate-edges [words fn1 fn2]
  "Returns lazy sequance of edges by applying
   fn1 and fn2 to each word. Each edge connects
   two vertices: (fn1 word) and (fn2 word)"
  (for [word words
        [from to] (map list [(fn1 word)] [(fn2 word)])]
    (new-edge from to)))





























