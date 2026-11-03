(require '[clojure.string :as str])
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          n (Integer/parseInt (nth toks 0)) f (Integer/parseInt (nth toks 1)) R (Integer/parseInt (nth toks 2))
          vals (mapv #(Double/parseDouble %) (subvec (vec toks) 3 (+ 3 n)))
          result (loop [v vals r 0]
                   (if (>= r R) v
                     (let [recvd (vec (sort v)) lo f hi (- n f)
                           avg (when (> hi lo) (/ (reduce + (subvec recvd lo hi)) (- hi lo)))
                           nv (mapv (fn [i] (if (<= hi lo) (nth v i) avg)) (range n))]
                       (recur nv (inc r)))))]
      (doseq [i (range n)] (println (fmt "P%d: %.6f" i (nth result i)))))))
(-main (slurp *in*))
