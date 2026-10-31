;; asynch_bellman_ford: asynchronous Bellman-Ford shortest paths via
;; super-round simulation; each node sends its distance on all outgoing
;; edges per round until quiescence (update messages only).
;; Batch 6 (T325). Input: n m, directed weighted edges, src.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0) m (nth toks 1)
            ev (subvec toks 2 (+ 2 (* 3 m)))
            eu (mapv #(nth ev (* 3 %)) (range m))
            vv (mapv #(nth ev (+ (* 3 %) 1)) (range m))
            ww (mapv #(nth ev (+ (* 3 %) 2)) (range m))
            src (nth toks (+ 2 (* 3 m)))
            INF 4000000000000000000
            res (loop [r 0 dist (assoc (vec (repeat n INF)) src 0) rounds 0]
                  (if (>= r (- n 1)) [dist rounds]
                    (let [[nd ch] (reduce (fn [[dd c] k]
                                            (let [du (nth dd (nth eu k))]
                                              (if (and (not= du INF) (< (+ du (nth ww k)) (nth dd (nth vv k))))
                                                [(assoc dd (nth vv k) (+ du (nth ww k))) true] [dd c])))
                                          [dist false] (range m))]
                      (if ch (recur (inc r) nd (inc rounds)) [nd (inc rounds)]))))
            [dist rounds] res
            neg (some (fn [k] (let [du (nth dist (nth eu k))]
                                (and (not= du INF) (< (+ du (nth ww k)) (nth dist (nth vv k))))))
                      (range m))]
        (if neg
          (do (println "NEGATIVE CYCLE") (println (str "MESSAGES: " (* m n))))
          (do (doseq [i (range n)]
                (let [di (nth dist i)]
                  (if (= di INF) (println (str i " INF")) (println (str i " " di)))))
              (println (str "MESSAGES: " (* m rounds)))))))))
(-main (slurp *in*))
