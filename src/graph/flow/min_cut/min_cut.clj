;; min-cut: s-t cut and Stoer-Wagner global cut
;;
;; Max-flow for the s-t cut plus Stoer-Wagner phases of maximum
;; adjacency orderings with contraction for the global cut. Emits
;; both values.
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
  (when (>= (count nums) 4)
    (let [n (nth nums 0) m (nth nums 1) s (nth nums 2) t (nth nums 3)
          es (mapv (fn [i] [(nth nums (+ 4 (* i 3))) (nth nums (+ 5 (* i 3))) (nth nums (+ 6 (* i 3)))]) (range m))
          res (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))]
      (doseq [[u v c] es]
        (swap! res assoc-in [u v] (+ (nth (nth @res u) v) c))
        (swap! res assoc-in [v u] (+ (nth (nth @res v) u) c)))
      (let [flow (loop [fl 0]
                   (let [par (atom (assoc (vec (repeat n -1)) s s))]
                     (loop [q [s]]
                       (when (and (seq q) (= (nth @par t) -1))
                         (let [u (first q)
                               new-vs (filterv (fn [v] (and (= (nth @par v) -1) (pos? (nth (nth @res u) v)))) (range n))]
                           (doseq [v new-vs] (swap! par assoc v u))
                           (recur (vec (concat (rest q) new-vs))))))
                     (if (= (nth @par t) -1)
                       fl
                       (let [b (loop [v t acc (bit-shift-left 1 60)]
                                 (if (= v s) acc (recur (nth @par v) (min acc (nth (nth @res (nth @par v)) v)))))]
                         (loop [v t]
                           (when (not= v s)
                             (let [u (nth @par v)]
                               (swap! res assoc-in [u v] (- (nth (nth @res u) v) b))
                               (swap! res assoc-in [v u] (+ (nth (nth @res v) u) b))
                               (recur u))))
                         (recur (+ fl b))))))
            w (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))
            alive (atom (vec (repeat n true)))
            best (atom (bit-shift-left 1 62))]
        (doseq [[u v c] es]
          (swap! w assoc-in [u v] (+ (nth (nth @w u) v) c))
          (swap! w assoc-in [v u] (+ (nth (nth @w v) u) c)))
        (loop [nn n]
          (when (> nn 1)
            (let [added (atom (vec (repeat n false)))
                  ws (atom (vec (repeat n 0)))
                  sv (atom [0 0])]
              (doseq [k (range nn)]
                (let [sel (loop [v 0 acc -1]
                            (if (>= v n) acc
                              (recur (inc v) (if (and (nth @alive v) (not (nth @added v))
                                                       (or (= acc -1) (> (nth @ws v) (nth @ws acc))))
                                                  v acc))))]
                  (swap! added assoc sel true)
                  (doseq [v (range n)]
                    (when (nth @alive v)
                      (swap! ws assoc v (+ (nth @ws v) (nth (nth @w sel) v)))))
                  (when (= k (- nn 2)) (swap! sv assoc 0 sel))
                  (when (= k (- nn 1)) (swap! sv assoc 1 sel))))
              (let [[pre-last last] @sv
                    coph (reduce + 0 (map #(nth (nth @w last) %) (filter #(and (nth @alive %) (not= % last)) (range n))))]
                (swap! best min coph)
                (doseq [v (range n)]
                  (swap! w assoc-in [pre-last v] (+ (nth (nth @w pre-last) v) (nth (nth @w last) v)))
                  (swap! w assoc-in [v pre-last] (+ (nth (nth @w v) pre-last) (nth (nth @w v) last))))
                (swap! alive assoc last false)
                (recur (dec nn))))))
        (println flow)
        (println @best)))))
