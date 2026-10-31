;; hash-table-open-addressing: linear probing + tombstones (CLRS 11.4).
;; Line1: m q. Ops: 1 k v put (FULL if full), 2 k get, 3 k delete,
;; 4 slot map ('.' empty, 'X' tombstone, 'O' occupied). Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(defn probe-put [st keys m start k]
  (loop [t 0 tomb -1]
    (if (>= t m) [-1 tomb]
      (let [s (mod (+ start t) m)]
        (cond (and (= (nth st s) 1) (= (nth keys s) k)) [s tomb]
              (= (nth st s) 0) [-1 (if (= tomb -1) s tomb)]
              :else (recur (+ t 1) (if (and (= (nth st s) 2) (= tomb -1)) s tomb)))))))

(defn probe-find [st keys m start k]
  (loop [t 0]
    (if (>= t m) -1
      (let [s (mod (+ start t) m)]
        (cond (= (nth st s) 0) -1
              (and (= (nth st s) 1) (= (nth keys s) k)) s
              :else (recur (+ t 1)))))))

(defn solve [toks]
  (let [m (read-int (nth toks 0)) q (read-int (nth toks 1))]
    (loop [k 0 i 2 st (vec (repeat m 0)) keys (vec (repeat m 0)) vals (vec (repeat m 0)) out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [kk (read-int (nth toks (+ i 1))) v (read-int (nth toks (+ i 2)))
                           [found tomb] (probe-put st keys m (mod kk m) kk)]
                       (cond (>= found 0) (recur (+ k 1) (+ i 3) st keys (assoc vals found v) out)
                             (>= tomb 0) (recur (+ k 1) (+ i 3) (assoc st tomb 1) (assoc keys tomb kk) (assoc vals tomb v) out)
                             :else (recur (+ k 1) (+ i 3) st keys vals (conj out "FULL"))))
            (= op 2) (let [kk (read-int (nth toks (+ i 1)))
                           f (probe-find st keys m (mod kk m) kk)]
                       (recur (+ k 1) (+ i 2) st keys vals (conj out (if (>= f 0) (str (nth vals f)) "NONE"))))
            (= op 3) (let [kk (read-int (nth toks (+ i 1)))
                           f (probe-find st keys m (mod kk m) kk)]
                       (if (>= f 0)
                         (recur (+ k 1) (+ i 2) (assoc st f 2) keys vals (conj out "OK"))
                         (recur (+ k 1) (+ i 2) st keys vals (conj out "NONE"))))
            (= op 4) (recur (+ k 1) (+ i 1) st keys vals
                            (conj out (apply str (mapv (fn [c] (cond (= c 0) \. (= c 1) \O :else \X)) st))))
            :else (recur (+ k 1) (+ i 1) st keys vals out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
