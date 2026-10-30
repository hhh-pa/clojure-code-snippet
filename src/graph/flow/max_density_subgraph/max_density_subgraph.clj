;; max-density-subgraph: densest subgraph via max-flow
;;
;; Goldberg reduction: min s-t cuts decide density thresholds, and
;; the first feasible candidate over exact rationals is the optimum.
;; Emits the reduced density fraction.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (let [done (reduce (fn [acc ch]
                       (if (contains? separators ch)
                         (if (empty? (:current acc))
                           acc
                           {:tokens (conj (:tokens acc) (apply str (:current acc)))
                             :current []})
                         (update acc :current conj ch)))
                     {:tokens [] :current []}
                     s)]
    (if (empty? (:current done))
      (:tokens done)
      (conj (:tokens done) (apply str (:current done))))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range; the bounds check
   runs before each multiply-and-add step so no step can overflow."
  [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs)
            neg? (= head \-)
            rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (>= acc (quot (+ min-long d) 10)))
                            (- (* acc 10) d)))))
                    0 rest-chs)
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (<= acc (quot (- max-long d) 10)))
                            (+ (* acc 10) d)))))
                    0 rest-chs)))))))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          es (mapv (fn [i] [(nth nums (+ 2 (* i 2))) (nth nums (+ 3 (* i 2)))]) (range m))
          deg (reduce (fn [acc [u v]] (-> acc (update u inc) (update v inc))) (vec (repeat n 0)) es)
          N (+ n 2) S n T (inc n)
          dinic (fn [cap0]
                  (let [cap (atom cap0) flow (atom 0)]
                    (loop []
                      (let [lvl (atom (assoc (vec (repeat N -1)) S 0))]
                        (loop [q [S]]
                          (when (seq q)
                            (let [u (first q)
                                  new-vs (filterv (fn [v] (and (= (nth @lvl v) -1) (pos? (nth (nth @cap u) v)))) (range N))]
                              (doseq [v new-vs] (swap! lvl assoc v (inc (nth @lvl u))))
                              (recur (vec (concat (rest q) new-vs))))))
                        (if (= (nth @lvl T) -1)
                          @flow
                          (let [it (atom (vec (repeat N 0)))
                                dfs (fn dfs [u f]
                                      (if (= u T) f
                                        (loop []
                                          (if (>= (nth @it u) N) 0
                                            (let [v (nth @it u)]
                                              (if (and (= (nth @lvl v) (inc (nth @lvl u))) (pos? (nth (nth @cap u) v)))
                                                (let [r (dfs v (min f (nth (nth @cap u) v)))]
                                                  (if (pos? r)
                                                    (do (swap! cap assoc-in [u v] (- (nth (nth @cap u) v) r))
                                                        (swap! cap assoc-in [v u] (+ (nth (nth @cap v) u) r))
                                                        r)
                                                    (do (swap! it assoc u (inc (nth @it u))) (recur))))
                                                (do (swap! it assoc u (inc (nth @it u))) (recur))))))))]
                            (loop []
                              (let [r (dfs S (bit-shift-left 1 60))]
                                (when (pos? r)
                                  (swap! flow + r)
                                  (recur))))
                            (recur)))))))
          cands (vec (sort-by (fn [[p q]] [(/ (double p) (double q)) p q])
                              (vec (for [p (range (inc m)) q (range 1 (inc n))] [p q]))))]
      (loop [cs cands prev nil]
        (if (empty? cs)
          (println "0/1")
          (let [[p q] (first cs)]
            (if (= [p q] prev)
              (recur (rest cs) prev)
              (let [cap (atom (mapv (fn [_] (mapv (fn [_] 0) (range N))) (range N)))]
                (doseq [v (range n)]
                  (swap! cap assoc-in [S v] (+ (nth (nth @cap S) v) (* m q)))
                  (swap! cap assoc-in [v T] (+ (nth (nth @cap v) T) (+ (* m q) (* 2 p) (- (* q (nth deg v)))))))
                (doseq [[u v] es]
                  (swap! cap assoc-in [u v] (+ (nth (nth @cap u) v) q))
                  (swap! cap assoc-in [v u] (+ (nth (nth @cap v) u) q)))
                (if (= (dinic @cap) (* q n m))
                  (let [g (loop [a p b q] (if (zero? b) a (recur b (mod a b))))]
                    (println (str (quot p g) "/" (quot q g))))
                  (recur (rest cs) [p q]))))))))))
