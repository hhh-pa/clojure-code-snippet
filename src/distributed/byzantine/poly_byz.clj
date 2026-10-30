(require '[clojure.string :as str])

(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          n (Integer/parseInt (nth toks 0)) f (Integer/parseInt (nth toks 1))
          init (mapv #(Integer/parseInt %) (subvec (vec toks) 2 (+ 2 n)))
          nf (Integer/parseInt (nth toks (+ 2 n)))
          faulty (let [ids (mapv #(Integer/parseInt %) (subvec (vec toks) (+ 3 n) (+ 3 n nf)))] (reduce (fn [s id] (assoc s id true)) (vec (repeat n false)) ids))]
      (let [fv (loop [val init ph 0]
                 (if (>= ph (inc f)) val
                     (let [king (mod ph n)
                           nval (mapv (fn [i] (if (nth faulty i) (nth val i) (let [cnt (frequencies (keep-indexed (fn [j v] (when-not (nth faulty j) v)) val)) [maj mc] (apply max-key second (seq cnt))] (if (>= mc (- n f)) maj (if (not (nth faulty king)) (nth val king) (nth val i))))))) (range n))]
                       (recur nval (inc ph)))))]
        (doseq [i (range n) :when (not (nth faulty i))] (println (str "P" i ": " (nth fv i))))))))

(-main (slurp *in*))
