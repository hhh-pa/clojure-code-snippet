;; atomic_snapshot: Afek et al. wait-free atomic snapshot (essence). Update
;; writes a component; scan double-collects with concurrent updates applied
;; between the collects; on mismatch the fresh view is borrowed (helping).
;; Batch 6 (T336). Input: n q, then U i v / S c [u v]* ops.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (Long/parseLong (nth toks 0)) q (Long/parseLong (nth toks 1))]
        (loop [k 0 p 2 state (vec (repeat n 0)) sn 0]
          (if (>= k q)
            (println (str "STATE:" (apply str (map #(str " " %) state))))
            (let [op (nth toks p)]
              (if (= op "U")
                (let [u (Long/parseLong (nth toks (+ p 1))) v (Long/parseLong (nth toks (+ p 2)))]
                  (recur (inc k) (+ p 3) (assoc state u v) sn))
                (let [c (Long/parseLong (nth toks (+ p 1)))
                      ups (mapv (fn [j] [(Long/parseLong (nth toks (+ p 2 (* 2 j))))
                                          (Long/parseLong (nth toks (+ p 3 (* 2 j))))]) (range c))
                      c1 state
                      c2 (reduce (fn [a [u v]] (assoc a u v)) state ups)
                      collects (if (= c1 c2) 2 3)]
                  (println (str "SCAN " sn ":" (apply str (map #(str " " %) c2)) " collects=" collects))
                  (recur (inc k) (+ p 2 (* 2 c)) c2 (inc sn)))))))))))
(-main (slurp *in*))
