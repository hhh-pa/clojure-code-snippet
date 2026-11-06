(require '[clojure.string :as str])
;; rle_compression: Run-length encoding of bitstream runs.
;; Pure Clojure with canonical input parsing.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn -main [input]
  (let [toks (filter seq (str/split input #"\s+")) s (first toks)]
    (when (seq s)
      (let [parts (partition-by identity s)
            encoded (map (fn [g] (str (first g) (count g))) parts)]
        (println (str/join " " encoded))))))
(-main (slurp *in*))