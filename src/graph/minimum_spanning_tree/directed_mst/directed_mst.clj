;; directed-mst: Chu-Liu/Edmonds minimum arborescence
;;
;; Repeatedly selects cheapest incoming edges, contracts directed
;; cycles with adjusted weights, and recurses. Emits the optimum
;; arborescence weight.
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
    (let [n0 (nth nums 0) m0 (nth nums 1) root0 (nth nums 2)
          es0 (mapv (fn [i] [(nth nums (+ 3 (* i 3))) (nth nums (+ 4 (* i 3))) (nth nums (+ 5 (* i 3)))]) (range m0))
          inf (bit-shift-left 1 60)]
      (loop [n n0 es es0 root root0 ans 0]
        (let [inw (atom (vec (repeat n inf)))
              pre (atom (vec (repeat n -1)))]
          (doseq [[u v w] es]
            (when (and (not= u v) (< w (nth @inw v)))
              (swap! inw assoc v w)
              (swap! pre assoc v u)))
          (swap! inw assoc root 0)
          (if (some #(= % inf) @inw)
            (println "NO ARBORESCENCE")
            (let [comp (atom (vec (repeat n -1)))
                  vis (atom (vec (repeat n -1)))
                  cn (atom 0)]
              (doseq [i (range n)]
                (let [v (loop [v i]
                           (if (and (not= (nth @vis v) i) (= (nth @comp v) -1) (not= v root))
                             (do (swap! vis assoc v i) (recur (nth @pre v)))
                             v))]
                  (when (and (not= v root) (= (nth @comp v) -1))
                    (loop [j (nth @pre v)]
                      (when (not= j v)
                        (swap! comp assoc j @cn)
                        (recur (nth @pre j))))
                    (swap! comp assoc v @cn)
                    (swap! cn inc))))
              (let [nans (+ ans (reduce + @inw))]
                (if (zero? @cn)
                  (println nans)
                  (do
                    (doseq [i (range n)]
                      (when (= (nth @comp i) -1)
                        (swap! comp assoc i @cn)
                        (swap! cn inc)))
                    (let [ne (filterv some?
                               (mapv (fn [[u v w]] (when (not= (nth @comp u) (nth @comp v))
                                                     [(nth @comp u) (nth @comp v) (- w (nth @inw v))])) es))]
                      (recur @cn ne (nth @comp root) nans))))))))))))
