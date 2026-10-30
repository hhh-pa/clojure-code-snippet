(ns solution (:require [clojure.string :as str]))
(defn -main []
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+")) s (first toks)]
    (when (seq s)
      (let [parts (partition-by identity s)
            encoded (map (fn [g] (str (first g) (count g))) parts)]
        (println (str/join " " encoded))))))
(-main)
