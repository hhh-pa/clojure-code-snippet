;; asynch_bfs: asynchronous BFS spanning tree. Root floods search; the first
;; search received sets parent and forwards; every search gets a response,
;; so each edge incident to the visited component carries 2 messages.
;; Batch 6 (T324). Input: n m, edges, root. Output: i parent dist + MESSAGES.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0) m (nth toks 1)
            ev (subvec toks 2 (+ 2 (* 2 m)))
            adj (reduce (fn [a k] (let [u (nth ev (* 2 k)) v (nth ev (+ (* 2 k) 1))]
                                    (-> a (update u (fnil conj []) v) (update v (fnil conj []) u))))
                        (vec (repeat n [])) (range m))
            root (nth toks (+ 2 (* 2 m)))
            INF 1000000000
            res (loop [front [root] vis (assoc (vec (repeat n false)) root true)
                       par (vec (repeat n -1)) dist (assoc (vec (repeat n INF)) root 0) d 0]
                  (if (empty? front) [vis par dist]
                    (let [[nv np nf] (reduce (fn [[v2 p2 nf2] u]
                                               (reduce (fn [[a b c] nb]
                                                         (if (nth a nb) [a b c]
                                                           [(assoc a nb true) (assoc b nb u) (conj c nb)]))
                                                       [v2 p2 nf2] (nth adj u)))
                                             [vis par []] front)
                          nd (reduce (fn [dd u] (assoc dd u d)) dist front)]
                      (recur nf nv np nd (inc d)))))
            [vis par dist] res
            msgs (* 2 (reduce + 0 (mapcat (fn [u] (if (nth vis u)
                                                   (map (fn [v] (if (or (< u v) (not (nth vis v))) 1 0)) (nth adj u))
                                                   [0])) (range n))))]
        (doseq [i (range n)]
          (println i (nth par i) (let [di (nth dist i)] (if (= di INF) -1 di))))
        (println (str "MESSAGES: " msgs))))))
(-main (slurp *in*))
