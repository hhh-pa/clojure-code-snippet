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

(defn three-way-qsort [a]
  (let [n (count a)]
    (if (<= n 1) a
      (persistent!
       (loop [arr (transient (vec a))
              stack (list [0 (dec n)])]
         (if (empty? stack)
           arr
           (let [[lo hi] (peek stack)
                 stack (pop stack)]
             (if (<= hi lo)
               (recur arr stack)
               (let [pivot (nth arr (+ lo (quot (inc (- hi lo)) 2)))
                     [arr lt gt]
                     (loop [arr arr lt lo i lo gt hi]
                       (if (> i gt)
                         [arr lt gt]
                         (let [x (nth arr i)]
                           (cond
                             (< x pivot)
                             (let [t (nth arr lt)]
                               (recur (assoc! arr lt x i t) (inc lt) (inc i) gt))
                             (> x pivot)
                             (let [t (nth arr gt)]
                               (recur (assoc! arr gt x i t) lt i (dec gt)))
                             :else (recur arr lt (inc i) gt)))))]
                 ;; Push the larger side first so the smaller side is
                 ;; processed first; stack depth stays O(log n).
                 (let [nl (- lt lo) nr (- hi gt)]
                   (recur arr (if (< nl nr)
                                (conj stack [(inc gt) hi] [lo (dec lt)])
                                (conj stack [lo (dec lt)] [(inc gt) hi])))))))))))))

(defn -main []
  (let [input (slurp *in*) toks (tokenize input) n (if (seq toks) (read-int (first toks)) 0)
        vals (mapv read-int (take n (rest toks))) sorted (three-way-qsort vals)]
    (print (str "[" (clojure.string/join "," sorted) "]"))))
(-main)
