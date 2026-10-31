;; floodmax: FloodMax & OptFloodMax synchronous general-network leader
;; election (Lynch 4.2). R = diameter rounds; FloodMax sends on every edge
;; every round; OptFloodMax resends only after the known max increases.
;; Batch 6 (T323). Input: n m, edges, ids. Output: leader/rounds/messages.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn bfs-dist [adj s n]
  (loop [d (assoc (vec (repeat n -1)) s 0) q (conj clojure.lang.PersistentQueue/EMPTY s)]
    (if (empty? q) d
      (let [u (peek q) q2 (pop q)]
        (recur (reduce (fn [dd v] (if (< (nth dd v) 0) (assoc dd v (inc (nth dd u))) dd)) d (nth adj u))
               (reduce (fn [qq v] (if (< (nth d v) 0) (conj qq v) qq)) q2 (nth adj u)))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0) m (nth toks 1)
            ev (subvec toks 2 (+ 2 (* 2 m)))
            adj (reduce (fn [a k] (let [u (nth ev (* 2 k)) v (nth ev (+ (* 2 k) 1))]
                                    (-> a (update u conj v) (update v conj u))))
                        (vec (repeat n [])) (range m))
            ids (subvec toks (+ 2 (* 2 m)) (+ 2 (* 2 m) n))
            mx (reduce max ids)
            R (reduce max 0 (mapcat #(bfs-dist adj % n) (range n)))
            seen0 ids]
        (loop [r 1 seen seen0 send (vec (repeat n true)) opt 0]
          (if (> r R)
            (do (println (str "LEADER: " mx)) (println (str "ROUNDS: " R))
                (println (str "FLOODMAX MESSAGES: " (* 2 m R)))
                (println (str "OPTFLOODMAX MESSAGES: " opt)))
            (let [ns (mapv (fn [u] (reduce (fn [a nb] (if (nth send nb) (max a (nth seen nb)) a))
                                           (nth seen u) (nth adj u))) (range n))
                  opt2 (+ opt (reduce + 0 (map (fn [u] (if (nth send u) (count (nth adj u)) 0)) (range n))))
                  send2 (mapv (fn [u] (not= (nth ns u) (nth seen u))) (range n))]
              (recur (inc r) ns send2 opt2))))))))
(-main (slurp *in*))
