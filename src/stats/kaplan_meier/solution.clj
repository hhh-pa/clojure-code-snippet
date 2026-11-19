;; Kaplan-Meier survival analysis (Think Stats 2e ch 13).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- km-curve
  [times events]
  (let [taus (vec (sort (set (map first (filter #(== (second %) 1)
                                                  (map vector times events))))))]
    (reduce (fn [[ss hs sc] tau]
              (let [d (count (filter true? (map (fn [t e] (and (== t tau) (== e 1)))
                                                     times events)))
                    nr (count (filter #(>= % tau) times))
                    s (* sc (/ (double (- nr d)) nr))]
                [(conj ss s) (conj hs (/ (double d) nr)) s]))
            [[] [] 1.0] taus)))

(defn- remaining
  [taus survs query]
  (if (or (empty? taus) (>= query (last taus)))
    0.0
    (let [sq (reduce (fn [acc [tau ss]] (if (<= tau query) ss (reduced acc)))
                     1.0 (map vector taus survs))]
      (if (== sq 0.0)
        0.0
        (let [[area _ _] (reduce (fn [[ar pt ps] [idx tau]]
                                   (if (> tau query)
                                     [(+ ar (* ps (- tau (if (> pt query) pt query))))
                                      tau (nth survs idx)]
                                     [ar tau (nth survs idx)]))
                                 [0.0 0 1.0] (map-indexed vector taus))]
          (/ area sq))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          times (mapv #(Long/parseLong (nth toks (+ 1 (* 2 %)))) (range n))
          events (mapv #(Long/parseLong (nth toks (+ 2 (* 2 %)))) (range n))
          q (Long/parseLong (nth toks (+ 1 (* 2 n))))
          queries (mapv #(Long/parseLong %) (subvec toks (+ 2 (* 2 n)) (+ 2 (* 2 n) q)))
          taus (vec (sort (set (map first (filter #(== (second %) 1)
                                                     (map vector times events))))))
          [survs hazards _] (km-curve times events)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          out (vec (concat (map (fn [tau sv hz] (str "SURV " tau " " (fmt sv) " " (fmt hz)))
                                 taus survs hazards)
                            (map (fn [qu] (str "REM " qu " " (fmt (remaining taus survs qu))))
                                 queries)))]
      (print (str/join "\n" out)))))
