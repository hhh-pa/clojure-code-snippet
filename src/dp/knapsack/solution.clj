(require '[clojure.string :as str])

;; 0-1 Knapsack with single-row transient DP table.
;; Time: O(t * n * s), Space: O(s).
;; Mirrors python-code-snippet/src/dp/knapsack/solution.py.

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [t (parse-long (nth toks 0))]
              (loop [case-i 0 idx 1]
                (when (< case-i t)
                  (let [s (parse-long (nth toks idx))
                        n (parse-long (nth toks (inc idx)))
                        dp (transient (vec (repeat (inc s) 0)))]
                    (loop [item-i 0 item-idx (+ idx 2)]
                      (when (< item-i n)
                        (let [w (parse-long (nth toks item-idx))
                              v (parse-long (nth toks (inc item-idx)))]
                          (when (<= w s)
                            (loop [j s]
                              (when (>= j w)
                                (let [nv (+ (nth dp (- j w)) v)]
                                  (when (> nv (nth dp j))
                                    (assoc! dp j nv)))
                                (recur (dec j)))))
                          (recur (inc item-i) (+ item-idx 2)))))
                    (println (nth dp s))
                    (recur (inc case-i) (+ idx 2 (* 2 n)))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
