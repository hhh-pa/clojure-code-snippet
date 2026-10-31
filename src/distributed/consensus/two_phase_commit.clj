(require '[clojure.string :as str])
(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          n (Integer/parseInt (nth toks 0))
          votes (subvec (vec toks) 1 (min (+ 1 n) (count toks)))
          all-yes (every? #(= % "YES") votes)
          dec (if all-yes "COMMIT" "ABORT")]
      (println (str "DECISION: " dec))
      (doseq [i (range n)] (let [s (if (= (nth votes i) "TIMEOUT") "ABORT" dec)] (println (str "P" i ": " s)))))))
(-main (slurp *in*))
