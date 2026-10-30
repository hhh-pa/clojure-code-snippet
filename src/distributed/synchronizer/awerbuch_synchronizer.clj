;; awerbuch_synchronizer: Awerbuch's alpha, beta and gamma network
;; synchronizers. Alpha acks every message (2m msgs/pulse, time 1); beta
;; convergecasts+broadcasts over a BFS tree (2(n-1) msgs, time 2*depth);
;; gamma runs alpha inside clusters + beta over the cluster tree.
;; Batch 6 (T337). Input: n m, edges, P, nc, clustering, cluster tree.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn bfs-depth [adj s n]
  (loop [d (assoc (vec (repeat n -1)) s 0) q (conj clojure.lang.PersistentQueue/EMPTY s) mx 0]
    (if (empty? q) mx
      (let [u (peek q)
            dd (reduce (fn [dd v] (if (< (nth dd v) 0) (assoc dd v (inc (nth dd u))) dd)) d (nth adj u))]
        (recur dd
               (reduce (fn [qq v] (if (< (nth d v) 0) (conj qq v) qq)) (pop q) (nth adj u))
               (reduce max mx (map #(nth dd %) (nth adj u))))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [t (mapv parse-token toks) p (atom 0)
            nxt (fn [] (let [v (nth t @p)] (swap! p inc) v))
            n (nxt) m (nxt)
            adj (loop [k 0 a (vec (repeat n [])) e []]
                  (if (>= k m) [a e]
                    (let [u (nxt) v (nxt)]
                      (recur (inc k) (-> a (update u conj v) (update v conj u)) (conj e [u v])))))
            [ad edges] adj P (nxt) nc (nxt)
            cl (mapv (fn [_] (nxt)) (range n))
            cad (loop [k 0 a (vec (repeat nc []))]
                  (if (>= k (max 0 (- nc 1))) a
                    (let [u (nxt) v (nxt)] (recur (inc k) (-> a (update u conj v) (update v conj u))))))
            depth (bfs-depth ad 0 n)
            cdepth (if (pos? nc) (bfs-depth cad 0 nc) 0)
            minintra (count (filter (fn [[u v]] (= (nth cl u) (nth cl v))) edges))]
        (println (str "ALPHA MESSAGES: " (* 2 m P) " TIME: " P))
        (println (str "BETA MESSAGES: " (* 2 (- n 1) P) " TIME: " (* 2 depth P)))
        (println (str "GAMMA MESSAGES: " (* (+ (* 2 minintra) (* 2 (- nc 1))) P)
                      " TIME: " (* (+ 2 (* 2 cdepth)) P)))))))
(-main (slurp *in*))
