(require '[clojure.string :as str])

(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          n (Integer/parseInt (nth toks 0)) m (Integer/parseInt (nth toks 1))
          et (subvec (vec toks) 2)
          edges (mapv (fn [i] [(Integer/parseInt (nth et (* i 3))) (Integer/parseInt (nth et (inc (* i 3)))) (Long/parseLong (nth et (+ 2 (* i 3))))]) (range m))
          src (Integer/parseInt (nth et (* m 3)))
          INF Long/MAX_VALUE
          dist (loop [d (assoc (vec (repeat n INF)) src 0) r 0]
                 (if (>= r (dec n)) d
                     (let [nd (reduce (fn [dd [u v w]] (if (and (not= (nth dd u) INF) (< (+ (nth dd u) w) (nth dd v))) (assoc dd v (+ (nth dd u) w)) dd)) d edges)]
                       (if (= nd d) d (recur nd (inc r))))))
          neg (some (fn [[u v w]] (and (not= (nth dist u) INF) (< (+ (nth dist u) w) (nth dist v)))) edges)]
      (if neg (println "NEGATIVE CYCLE")
          (doseq [i (range n)] (if (= (nth dist i) INF) (println i "INF") (println i (nth dist i))))))))

(-main (slurp *in*))
