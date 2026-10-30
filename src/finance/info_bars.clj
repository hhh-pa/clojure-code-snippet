;; info_bars: information-driven bars (AFML ch.2): tick bars (every N
;; trades), volume bars (every V shares), dollar bars (every D dollars),
;; tick-imbalance bars (|cum tick sign| >= T) and volume-run bars (|cum
;; signed volume| >= R). Counts complete bars; lastclose is the last
;; complete bar close (else first price). Batch 6 (T364).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn tick-sign [a b prev] (cond (> b a) 1 (< b a) -1 :else prev))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [N (Long/parseLong (nth toks 0)) V (Double/parseDouble (nth toks 1))
            D (Double/parseDouble (nth toks 2)) T (Double/parseDouble (nth toks 3))
            R (Double/parseDouble (nth toks 4)) M (Long/parseLong (nth toks 5))
            px (mapv (fn [k] (Double/parseDouble (nth toks (+ 6 (* 2 k))))) (range M))
            vol (mapv (fn [k] (Double/parseDouble (nth toks (+ 7 (* 2 k))))) (range M))
            nt (quot M N) tickc (if (pos? nt) (nth px (dec (* nt N))) (nth px 0))
            volres (loop [k 0 acc 0.0 nv 0 c (nth px 0)]
                     (if (>= k M) [nv c]
                       (let [a (+ acc (nth vol k))]
                         (if (>= a (- V 1e-12)) (recur (inc k) 0.0 (inc nv) (nth px k)) (recur (inc k) a nv c)))))
            dolres (loop [k 0 acc 0.0 nd 0 c (nth px 0)]
                     (if (>= k M) [nd c]
                       (let [a (+ acc (* (nth px k) (nth vol k)))]
                         (if (>= a (- D 1e-12)) (recur (inc k) 0.0 (inc nd) (nth px k)) (recur (inc k) a nd c)))))
            tibres (loop [k 1 cum 0.0 nb 0 c (nth px 0) prev 1]
                     (if (>= k M) [nb c]
                       (let [b (tick-sign (nth px (dec k)) (nth px k) prev) cu (+ cum b)]
                         (if (>= (Math/abs cu) (- T 1e-12))
                           (recur (inc k) 0.0 (inc nb) (nth px k) b) (recur (inc k) cu nb c b)))))
            vrunres (loop [k 1 cum 0.0 nb 0 c (nth px 0) prev 1]
                      (if (>= k M) [nb c]
                        (let [b (tick-sign (nth px (dec k)) (nth px k) prev) cu (+ cum (* b (nth vol k)))]
                          (if (>= (Math/abs cu) (- R 1e-12))
                            (recur (inc k) 0.0 (inc nb) (nth px k) b) (recur (inc k) cu nb c b)))))]
        (println (str "TICK: " nt " " (fmt "%.2f" tickc)))
        (println (str "VOLUME: " (nth volres 0) " " (fmt "%.2f" (nth volres 1))))
        (println (str "DOLLAR: " (nth dolres 0) " " (fmt "%.2f" (nth dolres 1))))
        (println (str "TIB: " (nth tibres 0) " " (fmt "%.2f" (nth tibres 1))))
        (println (str "VRUN: " (nth vrunres 0) " " (fmt "%.2f" (nth vrunres 1))))))))
(-main (slurp *in*))
