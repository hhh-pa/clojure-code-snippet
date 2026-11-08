(require '[clojure.string :as str])

;; Dijkstra shortest reach with a binary min-heap.
;; Time: O((V + E) log V), Space: O(V + E).
;; Mirrors python-code-snippet/src/graph/shortest_path/dijkstrashortreach.py.

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- min-heap-push! [heap entry]
  (let [n (inc (nth heap 0))]
    (assoc! heap 0 n)
    (loop [i n]
      (if (<= i 1)
        (assoc! heap 1 entry)
        (let [p (bit-shift-right i 1)
              pv (nth heap p)]
          (if (< (nth entry 0) (nth pv 0))
            (do (assoc! heap i pv)
                (recur p))
            (assoc! heap i entry)))))))

(defn- min-heap-pop! [heap]
  (let [n (nth heap 0)
        top (nth heap 1)
        last-v (nth heap n)
        n2 (dec n)]
    (assoc! heap 0 n2)
    (when (pos? n2)
      (loop [i 1]
        (let [l (bit-shift-left i 1)
              r (inc l)]
          (if (> l n2)
            (assoc! heap i last-v)
            (let [lv (nth heap l)
                  best-c (if (and (<= r n2) (< (nth (nth heap r) 0) (nth lv 0))) r l)
                  cv (nth heap best-c)]
              (if (< (nth cv 0) (nth last-v 0))
                (do (assoc! heap i cv)
                    (recur best-c))
                (assoc! heap i last-v)))))))
    top))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [queries (parse-long (nth toks 0))]
              (loop [q 0 idx 1]
                (when (< q queries)
                  (let [n (parse-long (nth toks idx))
                        m (parse-long (nth toks (inc idx)))
                        adj (transient (vec (repeat (inc n) [])))]
                    (loop [i 0 edge-idx (+ idx 2)]
                      (when (< i m)
                        (let [u (parse-long (nth toks edge-idx))
                              v (parse-long (nth toks (inc edge-idx)))
                              w (parse-long (nth toks (+ edge-idx 2)))]
                          (assoc! adj u (conj (nth adj u) [v w]))
                          (assoc! adj v (conj (nth adj v) [u w]))
                          (recur (inc i) (+ edge-idx 3)))))
                    (let [start-idx (+ idx 2 (* 3 m))
                          s (parse-long (nth toks start-idx))
                          dist (assoc! (transient (vec (repeat (inc n) -1))) s 0)
                          heap (transient [0])]
                      (min-heap-push! heap [0 s])
                      (loop []
                        (when (pos? (nth heap 0))
                          (let [[d u] (min-heap-pop! heap)]
                            (when (<= d (nth dist u))
                              (let [neighbors (nth adj u)]
                                (doseq [[v w] neighbors]
                                  (let [nd (+ d w)
                                        cur-d (nth dist v)]
                                    (when (or (= cur-d -1) (< nd cur-d))
                                      (assoc! dist v nd)
                                      (min-heap-push! heap [nd v]))))))
                            (recur))))
                      (let [row (loop [i 1 acc (transient [])]
                                  (if (> i n)
                                    (persistent! acc)
                                    (if (= i s)
                                      (recur (inc i) acc)
                                      (recur (inc i) (conj! acc (str (nth dist i)))))))]
                        (println (str/join " " row)))
                      (recur (inc q) (+ start-idx 1)))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
