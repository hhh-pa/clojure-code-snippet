;; weighted-median: O(n) 1-D facility location via weighted median
;; (Introduction to Algorithms, 4th Ed., Cormen et al., MIT Press, 2022,
;; Ch. 9.2 — Weighted median.)

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
                             {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs))))))))

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

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected integers"))
          (let [n (first nums)]
            (if (zero? n)
              (println 0)
              (let [vals (take n (rest nums))
                    wts (take n (drop (inc (count vals)) nums))
                    pairs (mapv vector vals wts)]
                (println (weighted-median pairs))))))))))

(-main (slurp *in*))
