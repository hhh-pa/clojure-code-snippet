;; cusum_filter: symmetric CUSUM filter (AFML ch.2). S+ = max(0, S+ + y),
;; S- = min(0, S- + y) with y_t = p_t - mean; an event fires when S+ > h
;; or S- < -h, then both reset. Batch 6 (T365). Input: h M, prices.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [h (Double/parseDouble (nth toks 0)) M (Long/parseLong (nth toks 1))
            px (mapv (fn [k] (Double/parseDouble (nth toks (+ 2 k)))) (range M))
            mu (/ (reduce + px) M)
            ev (loop [i 0 sp 0.0 sn 0.0 ev []]
                 (if (>= i M) ev
                   (let [y (- (nth px i) mu)
                         sp2 (max 0.0 (+ sp y)) sn2 (min 0.0 (+ sn y))]
                     (if (or (> sp2 h) (< sn2 (- h)))
                       (recur (inc i) 0.0 0.0 (conj ev i))
                       (recur (inc i) sp2 sn2 ev)))))]
        (println (str "EVENTS:" (apply str (map (fn [e] (str " " e)) ev))))
        (println (str "COUNT: " (count ev)))))))
(-main (slurp *in*))
