;; This file contains various input methods.
;;
;; General aim of this file functions is converting
;; some source of data into lazy sequance of strings.

(ns perfect_hashing.input
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn text->string-seq [text]
  "Generates lazy sequance of words from string sequance"
  (when-not (empty? text)
    (concat (filter #(seq %) (string/split (string/triml (first text)) #"\s+"))
      (lazy-seq (text->string-seq (rest text))))))

(defn input-from-file [filename]
  (let [rdr (io/reader filename)]
    (text->string-seq (line-seq rdr))))
