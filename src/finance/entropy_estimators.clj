;; entropy_estimators: plug-in histogram, LZ78-phrase (c*log2(n)/n) and
;; Kontoyiannis match-length (mean of log2(n)/L_i over second-half
;; positions with a prior match, else log2(n)) entropy estimators for
;; discrete symbol sequences. Batch 6 (T375). Input: M, symbols.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn log2 [x] (/ (Math/log x) (Math/log 2)))
(defn lz78 [s M]
  (loop [i 0 dict #{} c 0]
    (if (>= i M) c
      (let [j (loop [j i] (if (or (>= j M) (not (contains? dict (subvec s i (inc j))))) j (recur (inc j))))]
        (if (>= j M) (inc c) (recur (inc j) (conj dict (subvec s i (inc j))) (inc c)))))))
(defn klen [s p M]
  (loop [q 0 best 0]
    (if (>= q p) best
      (recur (inc q) (max best (loop [L 0] (if (and (< (+ p L) M) (= (nth s (+ q L)) (nth s (+ p L)))) (recur (inc L)) L)))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [M (Long/parseLong (nth toks 0))
            s (mapv (fn [k] (Long/parseLong (nth toks (+ 1 k)))) (range M))
            fr (frequencies s)
            plug (+ 0.0 (- (reduce + (map (fn [k] (let [pr (/ (double (get fr k)) M)] (* pr (log2 pr)))) (sort (keys fr))))))
            c (lz78 s M)
            lz (/ (* c (log2 (double M))) M)
            l2n (log2 (double M))
            ks (map (fn [p] (klen s p M)) (range (max 1 (quot M 2)) M))
            good (filter #(>= % 1) ks)
            kyv (if (seq good) (/ (reduce + (map (fn [L] (/ l2n L)) good)) (count good)) l2n)]
        (println (str "PLUGIN: " (fmt "%.6f" plug)))
        (println (str "LZ: " (fmt "%.6f" lz)))
        (println (str "KY: " (fmt "%.6f" kyv)))))))
(-main (slurp *in*))
