;; MSD radix sort: pure Clojure most-significant-digit string radix sort.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
                             {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done) (conj (:tokens done) (apply str (:current done))))))
(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))
(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [neg? (= \- (first chs)) pos? (= \+ (first chs))
            d0 (if (or neg? pos?) (rest chs) chs)]
        (when (seq d0)
          (loop [d d0 acc 0]
            (if (empty? d) (if neg? (- acc) acc)
              (let [v (get digit-values (first d))]
                (when v
                  (let [lim (if neg? min-long max-long)
                        nxt (if neg? (- (* acc 10) v) (+ (* acc 10) v))]
                    (when (or (= acc 0) (if neg? (<= nxt 0) (>= nxt 0)))
                      (recur (rest d) nxt))))))))))))
(defn read-int [tok] (parse-token tok))

(defn char-at [^String s ^long d] (if (< d (count s)) (int (.charAt s d)) -1))
(defn three-way-str-qsort [a d]
  (let [n (count a)]
    (if (<= n 1) a
      (let [^objects arr (object-array a)
            stack (java.util.ArrayDeque.)]
        (.push stack (long-array [0 (dec n) d]))
        (while (not (.isEmpty stack))
          (let [^longs rng (.pop stack)
                lo (aget rng 0) hi (aget rng 1) dd (aget rng 2)]
            (when (< lo hi)
              (let [pivot-c (char-at (aget arr lo) dd)]
                (loop [lt lo i lo gt hi]
                  (if (> i gt)
                    (do
                      (.push stack (long-array [lo (dec lt) dd]))
                      (.push stack (long-array [(inc gt) hi dd]))
                      (when (not= pivot-c -1)
                        (.push stack (long-array [lt gt (inc dd)]))))
                    (let [c (char-at (aget arr i) dd)]
                      (cond
                        (< c pivot-c)
                        (let [t (aget arr lt)]
                          (aset arr lt (aget arr i)) (aset arr i t)
                          (recur (inc lt) (inc i) gt))
                        (> c pivot-c)
                        (let [t (aget arr gt)]
                          (aset arr gt (aget arr i)) (aset arr i t)
                          (recur lt i (dec gt)))
                        :else (recur lt (inc i) gt)))))))))
        (vec arr)))))

(defn -main []
  (let [input (slurp *in*) lines (clojure.string/split-lines input)
        n (if (seq lines) (read-int (clojure.string/trim (first lines))) 0)
        vals (vec (take n (rest lines))) sorted (three-way-str-qsort vals 0)]
    (print (str "[" (clojure.string/join "," sorted) "]"))))
(-main)
