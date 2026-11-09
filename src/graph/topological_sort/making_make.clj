(require '[clojure.string :as str])

;; Topological order with max-first Kahn search.
;; Time: O((V + E) log V), Space: O(V + E).
;; Mirrors python-code-snippet/src/graph/topological_sort/making_make.py.

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- prepare
  "Parse every token of content with parse-long (nil for a bad token).
   Returning from this frame lets the token strings die before the search runs."
  [content]
  (mapv parse-long (tokenize content)))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- heap-push! [heap v]
  (let [n (inc (nth heap 0))]
    (assoc! heap 0 n)
    (loop [i n]
      (if (<= i 1)
        (assoc! heap 1 v)
        (let [p (bit-shift-right i 1)
              pv (nth heap p)]
          (if (< pv v)
            (do (assoc! heap i pv)
                (recur p))
            (assoc! heap i v)))))))

(defn- heap-pop! [heap]
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
                  best-c (if (and (<= r n2) (> (nth heap r) lv)) r l)
                  cv (nth heap best-c)]
              (if (> cv last-v)
                (do (assoc! heap i cv)
                    (recur best-c))
                (assoc! heap i last-v)))))))
    top))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [nums (prepare content)
              nt (count nums)]
          (when (pos? nt)
            (let [v (nth nums 0)
                  e (nth nums 1)]
              (if (or (nil? v) (nil? e) (neg? v) (neg? e))
                (report "invalid input: expected vertex and edge counts")
                (let [graph (transient (vec (repeat v [])))
                      indegree (transient (vec (repeat v 0)))]
                  (loop [k 0 idx 2]
                    (when (< k e)
                      (let [from (nth nums idx)
                            to (nth nums (inc idx))]
                        (assoc! graph from (conj (nth graph from) to))
                        (assoc! indegree to (inc (nth indegree to)))
                        (recur (inc k) (+ idx 2)))))
                  (let [heap (transient (vec (repeat (inc v) 0)))]
                    (loop [i 0]
                      (when (< i v)
                        (when (zero? (nth indegree i))
                          (heap-push! heap i))
                        (recur (inc i))))
                    (let [ans (loop [acc (transient [])]
                                (if (zero? (nth heap 0))
                                  (persistent! acc)
                                  (let [node (heap-pop! heap)
                                        neighbors (nth graph node)]
                                    (doseq [w neighbors]
                                      (let [nw (dec (nth indegree w))]
                                        (assoc! indegree w nw)
                                        (when (zero? nw)
                                          (heap-push! heap w))))
                                    (recur (conj! acc (str node))))))]
                      (println (str/join " " ans)))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
