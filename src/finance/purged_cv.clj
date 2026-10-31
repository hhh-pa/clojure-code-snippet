;; purged_cv: purged K-fold CV & combinatorial purged CV (AFML ch.7).
;; Folds partition label indices; training labels overlapping the test
;; time-span are purged, plus an embargo (labels starting within E bars
;; after test end). CPCV enumerates all C(P,k) test-partition combinations
;; in lexicographic order. Batch 6 (T369).
;; Input: purged P E / cpcv P k E, then L labels (t0 t1).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn next-comb [comb P k]
  (loop [i (dec k)]
    (cond (< i 0) nil
          (= (nth comb i) (- (+ P i) k)) (recur (dec i))
          :else (loop [j (inc i) c (assoc comb i (inc (nth comb i)))]
                  (if (>= j k) c (recur (inc j) (assoc c j (inc (nth c (dec j))))))))))
(defn all-combos [P k]
  (loop [comb (vec (range k)) out []]
    (let [out2 (conj out comb) nc (next-comb comb P k)]
      (if (nil? nc) out2 (recur nc out2)))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [mode (nth toks 0)
            [P k E q] (if (= mode "purged")
                        [(Long/parseLong (nth toks 1)) 1 (Long/parseLong (nth toks 2)) 3]
                        [(Long/parseLong (nth toks 1)) (Long/parseLong (nth toks 2))
                         (Long/parseLong (nth toks 3)) 4])
            L (Long/parseLong (nth toks q))
            t0 (mapv (fn [i] (Long/parseLong (nth toks (+ q 1 (* 2 i))))) (range L))
            t1 (mapv (fn [i] (Long/parseLong (nth toks (+ q 2 (* 2 i))))) (range L))
            parts (reduce (fn [a j] (update a (quot (* j P) L) conj j))
                          (vec (repeat P [])) (range L))
            tests (if (= mode "purged") (vec parts)
                    (mapv (fn [cb] (vec (mapcat #(nth parts %) cb))) (all-combos P k)))
            tag (if (= mode "purged") "FOLD " "SPLIT ")]
        (doseq [s (range (count tests))]
          (let [test (nth tests s)
                inset (set test)
                train (filterv (fn [j] (not (contains? inset j))) (range L))
                tmin (reduce min (map #(nth t0 %) test))
                tmax (reduce max (map #(nth t1 %) test))
                kept (filterv (fn [j] (and (or (< (nth t1 j) tmin) (> (nth t0 j) tmax))
                                              (not (and (> (nth t0 j) tmax) (<= (nth t0 j) (+ tmax E))))))
                              train)
                f (fn [v] (if (empty? v) "-" (str (nth v 0) (apply str (map #(str "," %) (subvec v 1))))))]
            (println (str tag s ": train=" (f kept) " test=" (f test)))))))))
(-main (slurp *in*))
