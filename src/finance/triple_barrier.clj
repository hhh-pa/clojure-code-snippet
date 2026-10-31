;; triple_barrier: triple-barrier labeling & meta-labeling (AFML ch.3).
;; For each event t0, scan t0+1..t0+H for first touch of +up (label +1) or
;; -dn (label -1), else 0 at the vertical barrier; meta label is 1 iff
;; side == label != 0. Batch 6 (T366). Input: M H up dn, prices, events.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn label-one [px t0 side H up dn M]
  (loop [t (inc t0)]
    (if (or (>= t M) (> t (+ t0 H))) [0 0]
      (let [r (- (nth px t) (nth px t0))]
        (cond (>= r (- up 1e-12)) [1 (if (= side 1) 1 0)]
              (<= r (+ (- dn) 1e-12)) [-1 (if (= side -1) 1 0)]
              :else (recur (inc t)))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [M (Long/parseLong (nth toks 0)) H (Long/parseLong (nth toks 1))
            up (Double/parseDouble (nth toks 2)) dn (Double/parseDouble (nth toks 3))
            px (mapv (fn [k] (Double/parseDouble (nth toks (+ 4 k)))) (range M))
            E (Long/parseLong (nth toks (+ 4 M)))
            evs (mapv (fn [j] [(Long/parseLong (nth toks (+ 5 M (* 2 j))))
                               (Long/parseLong (nth toks (+ 6 M (* 2 j))))]) (range E))
            res (mapv (fn [[t0 side]] (label-one px t0 side H up dn M)) evs)]
        (println (str "LABELS:" (apply str (map (fn [[l _]] (str " " l)) res))))
        (println (str "META:" (apply str (map (fn [[_ m]] (str " " m)) res))))))))
(-main (slurp *in*))
