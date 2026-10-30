;; median-of-two-sorted: O(log min(m,n)) median of two sorted arrays
;; via binary partition cut (Karumanchi / LeetCode 4). The smaller
;; array is binary searched; each cut leaves half the elements on
;; each side and the max-left / min-right invariant identifies the
;; median. O(log min(m,n)) time, O(1) space. Pure Clojure. Input:
;; m n, then A and B values; output: median as integer or .5.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
                             {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs))))))))

(defn find-median [A B]
  (let [[A B] (if (<= (count A) (count B)) [A B] [B A])
        m (count A) n (count B) total (+ m n)]
    (if (and (zero? m) (zero? n))
      0.0
      (loop [lo 0 hi m]
        (if (> lo hi)
          0.0
          (let [cutA (quot (+ lo hi) 2)
                cutB (- (quot (inc total) 2) cutA)
                maxLA (if (zero? cutA) Long/MIN_VALUE (nth A (dec cutA)))
                minRA (if (= cutA m) Long/MAX_VALUE (nth A cutA))
                maxLB (if (zero? cutB) Long/MIN_VALUE (nth B (dec cutB)))
                minRB (if (= cutB n) Long/MAX_VALUE (nth B cutB))]
            (if (and (<= maxLA minRB) (<= maxLB minRA))
              (if (odd? total)
                (double (max maxLA maxLB))
                (/ (+ (max maxLA maxLB) (min minRA minRB)) 2.0))
              (if (> maxLA minRB)
                (recur lo (dec cutA))
                (recur (inc cutA) hi)))))))))

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected integers"))
          (let [m (first nums) n (second nums)
                vals (drop 2 nums)
                A (sort (take m vals))
                B (sort (take n (drop m vals)))
                ans (find-median (vec A) (vec B))]
            (if (== ans (long ans))
              (println (long ans))
              (println ans))))))))

(-main (slurp *in*))
