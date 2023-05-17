(ns perfect_hashing.Pasher
  (:use perfect_hashing.assignment
        perfect_hashing.input)
  (:gen-class
   :name perfect_hashing.Pasher
   :prefix "pasher-"
   :state perfect
   :init init
   :constructors {[String] []}
   :methods [[get_hash [String] Long]
             [generate [String] Void]]))

(defn pasher-init [filename]
  [[] (atom (gimme-perfect (input-from-file filename)))])

(defn pasher-get_hash [this word]
  (@(.perfect this) word))

(defn pasher-generate [this filename]
  (reset! (.perfect this) (gimme-perfect (input-from-file filename))))
