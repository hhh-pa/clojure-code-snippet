;; time_slice_election: synchronous ring leader election via time-slicing
;; (known ID space: id i starts at round i*n; only min-id message completes)
;; and variable-speeds (id-i message advances 1 hop per i+1 rounds).
;; Batch 6 (T321). Input: n then n ids. Output: rounds/messages + leader.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0)
            ids (subvec toks 1 (+ 1 n))
            mn (reduce min ids)
            ts (+ (* mn n) n)
            vs (* n (+ mn 1))
            msgs (reduce (fn [a x] (if (= x mn) a (+ a (min n (quot vs (+ x 1)))))) n ids)]
        (println (str "TIME-SLICE ROUNDS: " ts " MESSAGES: " n))
        (println (str "VARIABLE-SPEED ROUNDS: " vs " MESSAGES: " msgs))
        (println (str "LEADER: " mn))))))
(-main (slurp *in*))
