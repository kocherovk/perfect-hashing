;; This file contains disjoint set data structure.

(ns perfect_hashing.disjoint_set)

(defn make-disjoint-set [n]
    "Creates new disjoint set with for n elements"
    (let [array   (int-array n)
          weights (int-array n)]
      (doseq [i (range n)]
        (aset array i i)
        (aset weights i 1))
      (fn [m]
        (cond
         (= m 0) array
         (= m 1) weights))))

(defn array [dset]
  (dset 0))

(defn weights [dset]
  (dset 1))

(let [parent (fn [dset x]
               (aget (array dset) x))

      weight (fn [dset x]
               (aget (weights dset) x))
      root (fn [dset x]
             (if (= x (parent dset x))
               x
               (do
                 (aset (array dset) x (parent dset x))
                 (recur dset (parent dset x)))))]

  (defn find? [dset x y]
    "Check if x and y is in one set"
    (= (root dset x) (root dset y)))

  (defn union [dset x y]
    "Union of two sets, wich contain x and y"
    (let [i (root dset x) j (root dset y)]
      (if (< (weight dset i) (weight dset j))
        (do
          (aset (array dset) i j)
          (aset (weights dset) j (+ (weight dset j) (weight dset i))))
        (do
          (aset (array dset) j i)
          (aset (weights dset) i (+ (weight dset j) (weight dset i))))))))
