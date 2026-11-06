;; huffman: optimal prefix code by greedy merging
;;
;; Merges the two lightest trees (ties by smallest leaf index) until
;; one tree remains. Emits the total weighted cost and each symbol code
;; length in input order.
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
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 1)
    (let [n (nth nums 0)
          fr (mapv #(nth nums (inc %)) (range n))]
      (if (<= n 1)
        (do (println 0) (println 0))
        (let [F (atom fr) MS (atom (vec (range n)))
              L (atom (mapv (fn [_] -1) (range n))) R (atom (mapv (fn [_] -1) (range n)))
              ;; total order on node ids: [weight, min-leaf, id]
              lt? (fn [Fv MSv a b]
                    (let [fa (nth Fv a) fb (nth Fv b)]
                      (if (not= fa fb) (< fa fb)
                        (let [ma (nth MSv a) mb (nth MSv b)]
                          (if (not= ma mb) (< ma mb) (< a b))))))
              hswap (fn [h i j] (let [t (nth h i)] (assoc h i (nth h j) j t)))
              hpush (fn [h Fv MSv id]
                      (loop [h (conj h id) i (dec (count h))]
                        (if (zero? i) h
                          (let [p (quot (dec i) 2)]
                            (if (lt? Fv MSv (nth h i) (nth h p))
                              (recur (hswap h i p) p) h)))))
              ;; pop the minimum; returns [min-id new-heap]
              hpop (fn [h Fv MSv]
                     (let [top (nth h 0) last (peek h) h2 (pop h)]
                       (if (empty? h2) [top h2]
                         (loop [h (assoc h2 0 last) i 0]
                           (let [l (+ 1 (* 2 i)) r (+ 2 (* 2 i)) c (count h)
                                 m (if (and (< l c) (lt? Fv MSv (nth h l) (nth h i))) l i)
                                 m (if (and (< r c) (lt? Fv MSv (nth h r) (nth h m))) r m)]
                             (if (= m i) [top h] (recur (hswap h i m) m)))))))
              ms0 (vec (range n))
              pq (atom (reduce (fn [h i] (hpush h fr ms0 i)) [] (range n)))]
          (loop []
            (when (> (count @pq) 1)
              (let [Fv @F MSv @MS
                    [a h1] (hpop @pq Fv MSv)
                    [b h2] (hpop h1 Fv MSv)
                    id (count Fv)
                    nF (conj Fv (+ (nth Fv a) (nth Fv b)))
                    nMS (conj MSv (min (nth MSv a) (nth MSv b)))]
                (reset! F nF) (reset! MS nMS)
                (swap! L conj a) (swap! R conj b)
                (reset! pq (hpush h2 nF nMS id))
                (recur))))
          (let [root (first @pq)
                dd (atom (mapv (fn [_] 0) (range (count @F))))]
            (loop [st [root]]
              (if (empty? st)
                nil
                (let [u (peek st) rest-st (pop st)]
                  (if (>= (nth @L u) 0)
                    (do
                      (swap! dd assoc (nth @L u) (inc (nth @dd u)))
                      (swap! dd assoc (nth @R u) (inc (nth @dd u)))
                      (recur (conj rest-st (nth @L u) (nth @R u))))
                    (recur rest-st)))))
            (println (reduce + (map * fr (subvec @dd 0 n))))
            (println (clojure.string/join " " (subvec @dd 0 n)))))))))
