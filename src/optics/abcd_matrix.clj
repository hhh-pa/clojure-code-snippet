(require '[clojure.string :as str])
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (when-not (str/blank? input)
    (let [lines (str/split-lines (str/trim input))
          m (Integer/parseInt (str/trim (nth lines 0)))
          elems (subvec (vec lines) 1 (+ 1 m))
          last-line (str/trim (nth lines (+ 1 m)))
          ltoks (str/split last-line #"\s+") y0 (Double/parseDouble (nth ltoks 0)) th0 (Double/parseDouble (nth ltoks 1))
          sys (reduce (fn [[sa sb sc sd] line]
                        (let [toks (str/split (str/trim line) #"\s+") kind (nth toks 0)
                              [ea eb ec ed] (cond (= kind "free") [1.0 (Double/parseDouble (nth toks 1)) 0.0 1.0]
                                                   (or (= kind "lens") (= kind "mirror")) [1.0 0.0 (/ -1.0 (Double/parseDouble (nth toks 1))) 1.0]
                                                   (= kind "refract") (let [n1 (Double/parseDouble (nth toks 1)) n2 (Double/parseDouble (nth toks 2)) R (Double/parseDouble (nth toks 3))] [1.0 0.0 (/ (- n1 n2) (* n2 R)) (/ n1 n2)])
                                                   :else [1.0 0.0 0.0 1.0])
                              na (+ (* ea sa) (* eb sc)) nb (+ (* ea sb) (* eb sd)) nc (+ (* ec sa) (* ed sc)) nd (+ (* ec sb) (* ed sd))]
                          [na nb nc nd])) [1.0 0.0 0.0 1.0] elems)
          [sa sb sc sd] sys y1 (+ (* sa y0) (* sb th0)) th1 (+ (* sc y0) (* sd th0))]
      (println (fmt "%.6f %.6f %.6f %.6f" sa sb sc sd))
      (println (fmt "%.6f %.6f" y1 th1)))))
(-main (slurp *in*))
