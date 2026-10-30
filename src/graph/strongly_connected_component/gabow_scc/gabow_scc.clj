;; gabow-scc: path-based strongly connected components
;;
;; Gabow DFS with vertex stack S and boundary stack P: preorder
;; numbers plus P-trimming on back edges emit each SCC at its root.
;; Prints the count and the min-vertex label per vertex.
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
  (let [done (reduce (fn [acc ch]
                       (if (contains? separators ch)
                         (if (empty? (:current acc))
                           acc
                           {:tokens (conj (:tokens acc) (apply str (:current acc)))
                             :current []})
                         (update acc :current conj ch)))
                     {:tokens [] :current []}
                     s)]
    (if (empty? (:current done))
      (:tokens done)
      (conj (:tokens done) (apply str (:current done))))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range; the bounds check
   runs before each multiply-and-add step so no step can overflow."
  [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs)
            neg? (= head \-)
            rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (>= acc (quot (+ min-long d) 10)))
                            (- (* acc 10) d)))))
                    0 rest-chs)
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (<= acc (quot (- max-long d) 10)))
                            (+ (* acc 10) d)))))
                    0 rest-chs)))))))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          adj (reduce (fn [acc i] (let [u (nth nums (+ 2 (* i 2))) v (nth nums (+ 3 (* i 2)))]
                                     (update acc u conj v)))
                       (mapv (fn [_] []) (range n)) (range m))
          pre (atom (vec (repeat n 0))) comp (atom (vec (repeat n -1)))
          S (atom []) P (atom []) C (atom 0)]
      (letfn [(dfs [v]
                (swap! C inc) (swap! pre assoc v @C)
                (swap! S conj v) (swap! P conj v)
                (doseq [w (nth adj v)]
                  (cond (= (nth @pre w) 0) (dfs w)
                        (= (nth @comp w) -1)
                        (loop []
                          (when (> (nth @pre (peek @P)) (nth @pre w))
                            (swap! P pop) (recur)))))
                (when (= (peek @P) v)
                  (swap! P pop)
                  (loop []
                    (let [x (peek @S)]
                      (swap! S pop) (swap! comp assoc x v)
                      (when (not= x v) (recur))))))]
        (doseq [v (range n)] (when (zero? (nth @pre v)) (dfs v)))
        (let [groups (reduce (fn [acc v] (update acc (nth @comp v) (fnil conj []) v)) {} (range n))
              lab (reduce (fn [acc vs] (let [mn (reduce min vs)] (reduce (fn [a v] (assoc a v mn)) acc vs)))
                          (vec (repeat n 0)) (vals groups))]
          (println (count groups))
          (println (clojure.string/join " " lab)))))))
