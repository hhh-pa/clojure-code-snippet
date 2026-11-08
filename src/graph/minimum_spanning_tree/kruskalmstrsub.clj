(require '[clojure.string :as str])

;; Kruskal's minimum spanning forest with union-find by size and path compression.
;; Time: O(E log E), Space: O(V + E).
;; Mirrors python-code-snippet/src/graph/minimum_spanning_tree/kruskalmstrsub.py.

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- find-root! [parent x]
  (let [root (loop [curr x]
               (let [p (nth parent curr)]
                 (if (= p curr) curr (recur p))))]
    (loop [curr x]
      (if (= curr root)
        root
        (let [nxt (nth parent curr)]
          (assoc! parent curr root)
          (recur nxt))))))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [g-nodes (parse-long (nth toks 0))
                  g-edges (parse-long (nth toks 1))]
              (if (or (nil? g-nodes) (nil? g-edges) (neg? g-nodes) (neg? g-edges))
                (report "invalid input: expected node and edge counts")
                (let [edges (loop [k 0 idx 2 acc (transient [])]
                              (if (= k g-edges)
                                (persistent! acc)
                                (let [u (parse-long (nth toks idx))
                                      v (parse-long (nth toks (inc idx)))
                                      w (parse-long (nth toks (+ idx 2)))]
                                  (recur (inc k) (+ idx 3) (conj! acc [w u v])))))
                      sorted-edges (vec (sort edges))
                      m (count sorted-edges)
                      parent (transient (vec (range (inc g-nodes))))
                      sizes (transient (vec (repeat (inc g-nodes) 1)))]
                  (loop [i 0 total 0]
                    (if (= i m)
                      (println total)
                      (let [[w u v] (nth sorted-edges i)
                            ru (find-root! parent u)
                            rv (find-root! parent v)]
                        (if (= ru rv)
                          (recur (inc i) total)
                          (let [su (nth sizes ru)
                                sv (nth sizes rv)]
                            (if (>= su sv)
                              (do (assoc! parent rv ru)
                                  (assoc! sizes ru (+ su sv))
                                  (recur (inc i) (+ total w)))
                              (do (assoc! parent ru rv)
                                  (assoc! sizes rv (+ su sv))
                                  (recur (inc i) (+ total w))))))))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
