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

(defn char-at [s d] (if (< d (count s)) (int (nth s d)) -1))
(defn three-way-str-qsort [a d]
  (let [n (count a)]
    (if (<= n 1) a
      (persistent!
       (loop [arr (transient (vec a))
              stack (list [0 (dec n) d])]
         (if (empty? stack)
           arr
           (let [[lo hi dd] (peek stack)
                 stack (pop stack)]
             (if (<= hi lo)
               (recur arr stack)
               (let [pivot-c (char-at (nth arr lo) dd)
                     [arr lt gt]
                     (loop [arr arr lt lo i lo gt hi]
                       (if (> i gt)
                         [arr lt gt]
                         (let [c (char-at (nth arr i) dd)]
                           (cond
                             (< c pivot-c)
                             (let [t (nth arr lt)]
                               (recur (assoc! arr lt (nth arr i) i t) (inc lt) (inc i) gt))
                             (> c pivot-c)
                             (let [t (nth arr gt)]
                               (recur (assoc! arr gt (nth arr i) i t) lt i (dec gt)))
                             :else (recur arr lt (inc i) gt)))))]
                 (recur arr (cond-> (conj stack [lo (dec lt) dd] [(inc gt) hi dd])
                              (not= pivot-c -1) (conj [lt gt (inc dd)]))))))))))))

(defn -main []
  (let [input (slurp *in*) lines (clojure.string/split-lines input)
        n (if (seq lines) (read-int (clojure.string/trim (first lines))) 0)
        vals (vec (take n (rest lines))) sorted (three-way-str-qsort vals 0)]
    (print (str "[" (clojure.string/join "," sorted) "]"))))
(-main)
