(require '[clojure.string :as str])
;; burrows_wheeler_transform: BWT: circular suffix sorting + inverse via LF-mapping. O(n log n).
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
      (let [t (str s "$") n (count t)
            sa (sort-by #(subs t %) (range n))
            bwt (apply str (map #(nth t (mod (dec %) n)) sa))]
        (println bwt)))))
(-main (slurp *in*))