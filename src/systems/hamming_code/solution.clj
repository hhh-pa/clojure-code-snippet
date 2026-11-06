;; hamming-code: SEC-DED syndrome decoding with overall parity.
;; Line1: q ops. E <data> -> codeword; C <code> ->
;; "<data> OK|FIXED|DED". Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-int [tok] (parse-token tok))

(defn npar [k]
  (loop [p 0] (if (< (bit-shift-left 1 p) (+ k p 1)) (recur (+ p 1)) p)))

(defn ispow2 [x] (and (pos? x) (zero? (bit-and x (- x 1)))))

(defn ham-encode [data]
  (let [k (count data) p (npar k) hlen (+ k p)
        h0 (loop [i 1 di 0 h (vec (repeat (+ hlen 1) 0))]
             (if (> i hlen) h
               (if (ispow2 i) (recur (+ i 1) di h)
                 (recur (+ i 1) (+ di 1) (assoc h i (if (= (nth data di) \1) 1 0))))))
        h (loop [b 0 hh h0]
            (if (>= b p) hh
              (let [s (loop [i 1 acc 0]
                        (if (> i hlen) acc
                          (recur (+ i 1) (if (pos? (bit-and i (bit-shift-left 1 b))) (bit-xor acc (nth hh i)) acc))))]
                (recur (+ b 1) (assoc hh (bit-shift-left 1 b) s)))))
        p0 (reduce bit-xor (subvec h 1))]
    (apply str (cons (if (= p0 1) \1 \0) (map (fn [i] (if (= (nth h i) 1) \1 \0)) (range 1 (+ hlen 1)))))))

(defn ham-decode [code]
  (let [hlen (- (count code) 1)
        h (vec (cons 0 (mapv (fn [i] (if (= (nth code i) \1) 1 0)) (range 1 (+ hlen 1)))))
        p0 (if (= (nth code 0) \1) 1 0)
        p (loop [pp 0] (if (<= (bit-shift-left 1 pp) hlen) (recur (+ pp 1)) pp))
        syn (loop [b 0 acc 0]
              (if (>= b p) acc
                (let [s (loop [i 1 a2 0]
                          (if (> i hlen) a2
                            (recur (+ i 1) (if (pos? (bit-and i (bit-shift-left 1 b))) (bit-xor a2 (nth h i)) a2))))]
                  (recur (+ b 1) (if (= s 1) (bit-or acc (bit-shift-left 1 b)) acc)))))
        tot (reduce bit-xor p0 (subvec h 1))
        [h2 status] (cond (and (zero? syn) (zero? tot)) [h "OK"]
                          (zero? syn) [h "FIXED"]
                          (not (zero? tot)) [(assoc h syn (bit-xor (nth h syn) 1)) "FIXED"]
                          :else [h "DED"])
        data (apply str (map (fn [i] (if (= (nth h2 i) 1) \1 \0))
                             (filter (fn [i] (not (ispow2 i))) (range 1 (+ hlen 1)))))]
    (str data " " status)))

(defn solve [toks]
  (let [q (read-int (nth toks 0))]
    (mapv (fn [qi] (let [op (nth toks (+ 1 (* qi 2))) w (nth toks (+ 2 (* qi 2)))]
                     (if (= op "E") (ham-encode w) (ham-decode w))))
          (range q))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
