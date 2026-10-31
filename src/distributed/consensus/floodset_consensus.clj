(require '[clojure.string :as str])

(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          n (Integer/parseInt (nth toks 0)) f (Integer/parseInt (nth toks 1))
          init (mapv #(Integer/parseInt %) (subvec (vec toks) 2 (+ 2 n)))
          nf (Integer/parseInt (nth toks (+ 2 n)))
          faulty (let [ids (mapv #(Integer/parseInt %) (subvec (vec toks) (+ 3 n) (+ 3 n nf)))] (reduce (fn [s id] (assoc s id true)) (vec (repeat n false)) ids))
          W0 (mapv (fn [i] #{(nth init i)}) (range n))
          W (loop [W W0 r 0] (if (>= r (inc f)) W (let [nW (mapv (fn [i] (if (nth faulty i) (nth W i) (reduce (fn [s j] (if (or (nth faulty j) (= i j)) s (into s (nth W j)))) (nth W i) (range n)))) (range n))] (recur nW (inc r)))))]
      (doseq [i (range n) :when (not (nth faulty i))] (println (str "P" i ": " (apply min (nth W i))))))))

(-main (slurp *in*))
