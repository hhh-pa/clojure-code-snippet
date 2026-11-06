;; min-cost-max-flow: successive shortest augmenting paths
;;
;; Potentials keep reduced costs non-negative across Dijkstra
;; augmentations from a Bellman-Ford start. Emits flow and cost.
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
          res (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))
          cost (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))]
      (doseq [i (range m)]
        (let [u (nth nums (+ 4 (* i 4))) v (nth nums (+ 5 (* i 4)))
              c (nth nums (+ 6 (* i 4))) w (nth nums (+ 7 (* i 4)))]
          (swap! res assoc-in [u v] (+ (nth (nth @res u) v) c))
          (swap! cost assoc-in [u v] w)
          (swap! cost assoc-in [v u] (- w))))
      (let [inf (bit-shift-left 1 60)
            pi (atom (vec (repeat n 0)))]
        (doseq [_ (range n) u (range n) v (range n)]
          (when (and (pos? (nth (nth @res u) v)) (< (+ (nth @pi u) (nth (nth @cost u) v)) (nth @pi v)))
            (swap! pi assoc v (+ (nth @pi u) (nth (nth @cost u) v)))))
        (loop [flow 0 tot 0]
          (let [dist (atom (assoc (vec (repeat n inf)) s 0))
                par (atom (vec (repeat n -1)))]
            (let [h (atom (sorted-set [0 s]))]
              (loop []
                (when (seq @h)
                  (let [[du u] (first @h)]
                    (swap! h disj [du u])
                    (if (not= du (nth @dist u))
                      (recur)
                      (do
                        (doseq [v (range n)]
                          (when (pos? (nth (nth @res u) v))
                            (let [rc (+ (nth (nth @cost u) v) (nth @pi u) (- (nth @pi v)))
                                  nd (+ (nth @dist u) rc)]
                              (when (< nd (nth @dist v))
                                (swap! dist assoc v nd)
                                (swap! par assoc v u)
                                (swap! h conj [nd v])))))
                        (recur)))))))
            (if (= (nth @par t) -1)
              (println (str flow " " tot))
              (do
                (doseq [v (range n)]
                  (when (< (nth @dist v) inf)
                    (swap! pi assoc v (+ (nth @pi v) (nth @dist v)))))
                (let [b (loop [v t acc inf]
                          (if (= v s) acc (recur (nth @par v) (min acc (nth (nth @res (nth @par v)) v)))))
                      nt (atom tot)]
                  (loop [v t]
                    (when (not= v s)
                      (let [u (nth @par v)]
                        (swap! res assoc-in [u v] (- (nth (nth @res u) v) b))
                        (swap! res assoc-in [v u] (+ (nth (nth @res v) u) b))
                        (swap! nt + (* b (nth (nth @cost u) v)))
                        (recur u))))
                  (recur (+ flow b) @nt))))))))))
