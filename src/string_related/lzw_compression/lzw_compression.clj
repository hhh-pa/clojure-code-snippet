(ns solution (:require [clojure.string :as str]))
(defn lzw-encode [s]
  (let [init-d (into {} (for [i (range 256)] [(str (char i)) i]))]
    (loop [chars (seq s) w "" d init-d nxt 256 codes []]
      (if (empty? chars)
        (if (seq w) (conj codes (get d w)) codes)
        (let [c (str (first chars)) wc (str w c)]
          (if (contains? d wc)
            (recur (rest chars) wc d nxt codes)
            (recur (rest chars) c (assoc d wc nxt) (inc nxt) (conj codes (get d w)))))))))
(defn -main []
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+")) s (first toks)]
    (when (seq s)
      (println (str/join " " (lzw-encode s))))))
(-main)
