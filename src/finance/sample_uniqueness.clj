;; sample_uniqueness: sample uniqueness & sequential bootstrap (AFML
;; ch.4). Label i over bars [t0,t1] has uniqueness u_i = mean over t of
;; 1/c_t (c_t = concurrency); sequential bootstrap draws K labels with
;; probability proportional to u (fixed-seed LCG), then debits overlapping
;; labels' uniqueness (clamped at 0). Batch 6 (T367).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn lcg-next [x] (unchecked-add (unchecked-multiply x 6364136223846793005) 1442695040888963407))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [L (Long/parseLong (nth toks 0)) K (Long/parseLong (nth toks 1))
            t0 (mapv (fn [i] (Long/parseLong (nth toks (+ 2 (* 2 i))))) (range L))
            t1 (mapv (fn [i] (Long/parseLong (nth toks (+ 3 (* 2 i))))) (range L))
            Tmax (reduce max t1)
            c (reduce (fn [a i] (reduce (fn [b t] (update b t inc)) a (range (nth t0 i) (inc (nth t1 i)))))
                      (vec (repeat (inc Tmax) 0)) (range L))
            u (mapv (fn [i] (/ (reduce + (map (fn [t] (/ 1.0 (nth c t))) (range (nth t0 i) (inc (nth t1 i)))))
                               (- (nth t1 i) (nth t0 i) -1))) (range L))]
        (println (str "UNIQ:" (apply str (map (fn [x] (str " " (fmt "%.6f" x))) u))))
        (loop [d 0 lcg 999 uu u draws []]
          (if (>= d K)
            (println (str "BOOT:" (apply str (map (fn [x] (str " " x)) draws))))
            (let [tot (reduce + uu)
                  l2 (lcg-next lcg)
                  pick (if (<= tot 0) (Long/remainderUnsigned (inc (unsigned-bit-shift-right l2 11)) L)
                         (let [r (* (/ (double (inc (unsigned-bit-shift-right l2 11))) 9007199254740992.0) tot)]
                           (loop [i 0 acc 0.0]
                             (if (>= i L) (dec L)
                               (let [a (+ acc (nth uu i))] (if (> a r) i (recur (inc i) a)))))))
                  uu2 (reduce (fn [a t]
                                (reduce (fn [b j]
                                          (if (and (<= (nth t0 j) t) (<= t (nth t1 j)))
                                            (let [v (- (nth b j) (/ (/ 1.0 (nth c t)) (- (nth t1 j) (nth t0 j) -1)))]
                                              (assoc b j (if (< v 0) 0.0 v))) b))
                                        a (range L)))
                              uu (range (nth t0 pick) (inc (nth t1 pick))))]
              (recur (inc d) l2 uu2 (conj draws pick)))))))))
(-main (slurp *in*))
