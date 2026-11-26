;; Fabry-Perot etalon transmission (wave ch 3).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- snap [x] (if (< (Math/abs x) 5e-7) 0.0 x))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          r (Double/parseDouble (nth toks 0))
          n (Double/parseDouble (nth toks 1))
          d (Double/parseDouble (nth toks 2))
          theta (Double/parseDouble (nth toks 3))
          m (Long/parseLong (nth toks 4))
          lams (mapv #(Double/parseDouble %) (subvec toks 5 (+ 5 m)))
          fcoef (/ (* 4 r) (* (- 1 r) (- 1 r)))
          finesse (/ (* Math/PI (Math/sqrt r)) (- 1 r))
          trad (/ (* theta Math/PI) 180)
          cost (Math/cos trad)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          out (vec (cons (str "FINESSE " (fmt (snap finesse)))
                         (map (fn [lam]
                                (let [k (/ (* 4 Math/PI) lam)
                                      de (* (* (* k n) d) cost)
                                      sn (Math/sin (/ de 2))
                                      t (/ 1 (+ 1 (* (* fcoef sn) sn)))]
                                  (str "T " (fmt (snap t)))))
                              lams)))]
      (print (str/join "\n" out)))))
