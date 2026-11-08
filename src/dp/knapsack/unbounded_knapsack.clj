(require '[clojure.string :as str])

;; Unbounded knapsack with 1D DP table.
;; Time: O(t * n * s), Space: O(s).
;; Mirrors python-code-snippet/src/dp/knapsack/unbounded_knapsack.py.

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
                  (let [n (parse-long (nth toks idx))
                        s (parse-long (nth toks (inc idx)))
                        weights (loop [i 0 acc (transient [])]
                                  (if (= i n)
                                    (persistent! acc)
                                    (recur (inc i) (conj! acc (parse-long (nth toks (+ idx 2 i)))))))
                        dp (transient (vec (repeat (inc s) 0)))]
                    (loop [i 0]
                      (when (<= i s)
                        (let [cur (nth dp i)
                              best (loop [k 0 c cur]
                                     (if (= k n)
                                       c
                                       (let [w (nth weights k)]
                                         (if (<= w i)
                                           (let [val (+ (nth dp (- i w)) w)]
                                             (recur (inc k) (if (> val c) val c)))
                                           (recur (inc k) c)))))]
                          (assoc! dp i best)
                          (recur (inc i)))))
                    (println (nth dp s))
                    (recur (inc case-i) (+ idx 2 n))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
