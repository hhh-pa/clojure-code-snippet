(require '[clojure.string :as str])

;; Shortest reach in an unweighted graph with edge weight six.
;; A queue-based breadth-first search from the start node sets each distance once,
;; which takes O(V+E) time and space.
;; Mirrors python-code-snippet/src/search/bfs/ctci_bfs_shortest_reach.py.

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

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [nums (prepare content)
              nt (count nums)]
          (when (pos? nt)
            (let [queries (nth nums 0)]
              (loop [q 0 idx 1]
                (when (< q queries)
                  (let [n (nth nums idx)
                        m (nth nums (inc idx))
                        adj (transient (vec (repeat n [])))]
                    (loop [i 0 edge-idx (+ idx 2)]
                      (when (< i m)
                        (let [u (dec (nth nums edge-idx))
                              v (dec (nth nums (inc edge-idx)))]
                          (assoc! adj u (conj (nth adj u) v))
                          (assoc! adj v (conj (nth adj v) u))
                          (recur (inc i) (+ edge-idx 2)))))
                    (let [start-tok-idx (+ idx 2 (* 2 m))
                          start (dec (nth nums start-tok-idx))
                          dist (assoc! (transient (vec (repeat n -1))) start 0)
                          queue (transient [start])]
                      (loop [qi 0 qlen 1]
                        (when (< qi qlen)
                          (let [cur (nth queue qi)
                                nd (+ (nth dist cur) 6)
                                neighbors (nth adj cur)
                                num-nb (count neighbors)
                                nqlen (loop [k 0 ql qlen]
                                        (if (= k num-nb)
                                          ql
                                          (let [nxt (nth neighbors k)]
                                            (if (= (nth dist nxt) -1)
                                              (do
                                                (assoc! dist nxt nd)
                                                (conj! queue nxt)
                                                (recur (inc k) (inc ql)))
                                              (recur (inc k) ql)))))]
                            (recur (inc qi) nqlen))))
                      (let [row (loop [i 0 acc (transient [])]
                                  (if (= i n)
                                    (persistent! acc)
                                    (if (= i start)
                                      (recur (inc i) acc)
                                      (recur (inc i) (conj! acc (str (nth dist i)))))))]
                        (println (str/join " " row)))
                      (recur (inc q) (+ start-tok-idx 1)))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
