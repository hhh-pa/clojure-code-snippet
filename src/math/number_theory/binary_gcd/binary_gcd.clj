;; binary_gcd: binary GCD via Stein's shift-only algorithm. O(log max(a,b)).
;; Pure Clojure with canonical input parsing.

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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn bgcd [a b]
  (cond (zero? a) (Math/abs b) (zero? b) (Math/abs a)
        :else (let [a (Math/abs a) b (Math/abs b)
                    shift (loop [s 0 aa a bb b] (if (and (even? aa) (even? bb)) (recur (inc s) (quot aa 2) (quot bb 2)) s))
                    a (loop [x (quot a (bit-shift-left 1 shift))] (if (even? x) (recur (quot x 2)) x))
                    b (quot b (bit-shift-left 1 shift))]
                (loop [a a b b]
                  (if (zero? b) (bit-shift-left a shift)
                    (let [b (loop [x b] (if (even? x) (recur (quot x 2)) x))]
                      (if (> a b) (recur b (- a b)) (recur a (- b a)))))))))
;; simpler iterative version
(defn bgcd2 [a b]
  (let [a (Math/abs (long a)) b (Math/abs (long b))]
    (cond (zero? a) b (zero? b) a
          :else (loop [a a b b shift 0]
                  (cond (and (even? a) (even? b)) (recur (quot a 2) (quot b 2) (inc shift))
                        (even? a) (recur (quot a 2) b shift)
                        (even? b) (recur a (quot b 2) shift)
                        (> a b) (recur b (- a b) shift)
                        (= a b) (bit-shift-left a shift)
                        :else (recur a (- b a) shift))))))
(defn -main [input]
  (let [toks (tokenize input)
        vals (mapv #(Long/parseLong %) toks)]
    (cond (= 2 (count vals)) (println (bgcd2 (nth vals 0) (nth vals 1)))
          :else (let [t (first vals)]
                  (if (and (pos? t) (= (count vals) (inc (* 2 t))))
                    (doseq [i (range t)] (println (bgcd2 (nth vals (+ 1 (* 2 i))) (nth vals (+ 2 (* 2 i))))))
                    (doseq [[a b] (partition 2 vals)] (println (bgcd2 a b))))))))
(-main (slurp *in*))

