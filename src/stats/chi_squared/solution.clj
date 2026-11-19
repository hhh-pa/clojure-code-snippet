;; Chi-squared tests (Think Stats 2e ch 14).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          [stat dof] (if (= (nth toks 0) "GOF")
                       (let [k (Long/parseLong (nth toks 1))
                             obs (mapv #(Long/parseLong %) (subvec toks 2 (+ 2 k)))
                             probs (mapv #(Double/parseDouble %) (subvec toks (+ 2 k) (+ 2 (* 2 k))))
                             n (reduce + obs)
                             stat (reduce + (map (fn [o pr]
                                                   (let [e (* (double n) pr)
                                                         d (- (double o) e)]
                                                     (/ (* d d) e)))
                                                 obs probs))]
                         [stat (dec k)])
                       (let [r (Long/parseLong (nth toks 1))
                             c (Long/parseLong (nth toks 2))
                             cells (mapv #(Long/parseLong %) (subvec toks 3 (+ 3 (* r c))))
                             total (reduce + cells)
                             rows (mapv (fn [i] (reduce + (subvec cells (* i c) (* (inc i) c))))
                                        (range r))
                             cols (mapv (fn [j] (reduce + (map #(nth cells (+ (* % c) j)) (range r))))
                                        (range c))
                             stat (reduce + (map (fn [[i j]]
                                                   (let [e (/ (double (* (nth rows i) (nth cols j)))
                                                              total)]
                                                     (if (== e 0.0)
                                                       0.0
                                                       (let [d (- (double (nth cells (+ (* i c) j))) e)]
                                                         (/ (* d d) e)))))
                                                 (mapcat (fn [i] (map (fn [j] [i j]) (range c))) (range r))))]
                         [stat (* (dec r) (dec c))]))]
      (print (str "STAT " (fmt stat) "\nDOF " dof)))))
