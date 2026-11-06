;; dag-shortest-paths: shortest and longest paths in a DAG
;;
;; Kahn topological order then one relaxation pass for shortest
;; paths and one for longest (critical path). Emits both rows, with
;; INF markers for the unreachable.
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
  (when (>= (count nums) 3)
    (let [n (nth nums 0) m (nth nums 1) s (nth nums 2)
          adj (reduce (fn [acc i] (let [u (nth nums (+ 3 (* i 3))) v (nth nums (+ 4 (* i 3))) w (nth nums (+ 5 (* i 3)))]
                                       (update acc u conj [v w])))
                       (mapv (fn [_] []) (range n)) (range m))
          ind (atom (vec (repeat n 0)))
          _ (doseq [i (range m)] (let [v (nth nums (+ 4 (* i 3)))] (swap! ind assoc v (inc (nth @ind v)))))
          inf (bit-shift-left 1 60)
          topo (let [h (atom (sorted-set))
                       seen (atom (vec (repeat n false)))]
                   (doseq [v (range n)]
                     (when (zero? (nth @ind v))
                       (swap! h conj v)
                       (swap! seen assoc v true)))
                   (loop [acc []]
                     (if (empty? @h)
                       acc
                       (let [u (first @h)]
                         (swap! h disj u)
                         (doseq [[v _] (nth adj u)]
                           (swap! ind assoc v (dec (nth @ind v)))
                           (when (and (zero? (nth @ind v)) (not (nth @seen v)) (not= v u))
                             (swap! seen assoc v true)
                             (swap! h conj v)))
                         (recur (conj acc u))))))
          ds (atom (assoc (vec (repeat n inf)) s 0))
          dl (atom (assoc (vec (repeat n (- inf))) s 0))]
      (doseq [u topo [v w] (nth adj u)]
        (when (< (+ (nth @ds u) w) (nth @ds v))
          (swap! ds assoc v (+ (nth @ds u) w)))
        (when (and (> (nth @dl u) (quot (- inf) 2)) (> (+ (nth @dl u) w) (nth @dl v)))
          (swap! dl assoc v (+ (nth @dl u) w))))
      (println (clojure.string/join " " (map #(if (>= % (quot inf 2)) "INF" (str %)) @ds)))
      (println (clojure.string/join " " (map #(if (<= % (quot (- inf) 2)) "-INF" (str %)) @dl))))))
