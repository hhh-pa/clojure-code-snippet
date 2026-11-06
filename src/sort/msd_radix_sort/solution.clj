;; MSD radix sort: pure Clojure most-significant-digit string radix sort.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))
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

(defn -main []
  (let [input (slurp *in*) lines (clojure.string/split-lines input)
        n (if (seq lines) (read-int (clojure.string/trim (first lines))) 0)
        vals (vec (take n (rest lines))) sorted (sort vals)]
    (print (str "[" (clojure.string/join "," sorted) "]"))))
(-main)
