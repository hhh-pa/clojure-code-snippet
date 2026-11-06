;; weighted-median: O(n) 1-D facility location via weighted median
;; (Introduction to Algorithms, 4th Ed., Cormen et al., MIT Press, 2022,
;; Ch. 9.2 — Weighted median.)

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

(defn weighted-median
  "Weighted median: sum_{x_i < p} w_i < 0.5 and sum_{x_i > p} w_i <= 0.5."
  [pairs]
  (if (empty? pairs)
    0
    (loop [a (vec pairs) w-left 0.0]
      (if (= (count a) 1)
        (first (first a))
        (let [n (count a)
              s (sort-by first a)
              pivot (first (nth s (quot n 2)))
              less (filterv #(< (first %) pivot) a)
              eq (filterv #(= (first %) pivot) a)
              greater (filterv #(> (first %) pivot) a)
              w-less (reduce + (map second less))
              w-eq (reduce + (map second eq))]
          (cond
            (and (< (+ w-left w-less) 0.5) (>= (+ w-left w-less w-eq) 0.5)) pivot
            (>= (+ w-left w-less) 0.5) (recur less w-left)
            :else (recur greater (+ w-left w-less w-eq))))))))

(defn parse-number [tok]
  (try (Long/parseLong tok)
       (catch Exception _ (try (Double/parseDouble tok) (catch Exception _ nil)))))

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-number toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected numbers"))
          (let [n (long (first nums))]
            (if (zero? n)
              (println 0)
              (let [vals (take n (rest nums))
                    wts (take n (drop (inc (count vals)) nums))
                    pairs (mapv vector vals wts)]
                (println (weighted-median pairs))))))))))

(-main (slurp *in*))
