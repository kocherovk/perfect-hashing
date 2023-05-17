(defproject perfect-hashing "0.1"
  :description "Clojure project for building
                minimal perfect hash function
                for set of strings"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main perfect_hashing.core
  :aot [perfect_hashing.core perfect_hashing.Pasher]
  :target-path "target/%s")


