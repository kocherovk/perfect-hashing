(ns perfect_hashing.core
  (:use perfect_hashing.input
        perfect_hashing.graph
        perfect_hashing.mapping
        perfect_hashing.disjoint_set
        perfect_hashing.assignment
        perfect_hashing.util)
  (:gen-class))

(let [perfect (atom nil)]
  (defn -main [& args]
    (println
"
Welcome to pasher!

Please use \"generate\" command to create perfect hashing function:
user-> generate <filename>.

And then you may ask program to calculate hash of paricular word using \"hash\" command:
user-> hash <word>.

To stop program type \"exit\".
")
    (loop []
      (let [cmd (clojure.string/split (read-line) #" +")]
        (if (= (first cmd) "exit")
          nil
          (do
            (cond
             (= (first cmd) "generate") (try
                                          (if (second cmd)
                                            (do
                                              (println "Building function...")
                                              (reset! perfect (gimme-perfect (input-from-file (second cmd))))
                                              (println "Done!"))
                                            (println "Filename not found. Please try again."))
                                          (catch java.io.FileNotFoundException e (println "No such file(" (second cmd) ") exist!")))
             (= (first cmd) "hash") (if (nil? @perfect)
                                      (println "No perfect function was found. Please use ""generate"" command to build one.")
                                      (println (@perfect (second cmd))))
             :else (println "Unknown command. Please try again"))
            (recur)))))))




