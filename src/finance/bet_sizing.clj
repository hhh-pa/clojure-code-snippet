;; bet_sizing: bet sizing from predicted probabilities & dynamic active
;; averaging (AFML ch.10, essence). Signed size s = 2p - 1; the active
;; position is the decay-weighted average pos = d*pos + (1-d)*s clamped to
;; [-1, 1]. Batch 6 (T371). Input: n d, probabilities.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (Long/parseLong (nth toks 0)) d (Double/parseDouble (nth toks 1))
            pr (mapv (fn [k] (Double/parseDouble (nth toks (+ 2 k)))) (range n))
            s (mapv (fn [p] (- (* 2.0 p) 1.0)) pr)
            act (loop [i 0 pos 0.0 out []]
                  (if (>= i n) out
                    (let [p2 (max -1.0 (min 1.0 (+ (* d pos) (* (- 1.0 d) (nth s i)))))]
                      (recur (inc i) p2 (conj out p2)))))]
        (println (str "SIZES:" (apply str (map (fn [v] (str " " (fmt "%.6f" v))) s))))
        (println (str "ACTIVE:" (apply str (map (fn [v] (str " " (fmt "%.6f" v))) act))))))))
(-main (slurp *in*))
