;; min-path-cover: DAG path cover by bipartite matching
;;
;; Kuhn bipartite matching between left and right copies of the
;; DAG, then chains follow the matched links. Emits the cover size
;; and the sorted paths.
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
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          adj (reduce (fn [acc i] (let [u (nth nums (+ 2 (* i 2))) v (nth nums (+ 3 (* i 2)))]
                                     (update acc u conj v)))
                       (mapv (fn [_] []) (range n)) (range m))
          adj (mapv (fn [u] (vec (sort (nth adj u)))) (range n))
          mtR (atom (vec (repeat n -1)))]
      (letfn [(bpm [u seen]
                (loop [vs (nth adj u) sn seen]
                  (if (empty? vs) [false sn]
                    (let [v (first vs)]
                      (if (nth sn v)
                        (recur (rest vs) sn)
                        (let [sn2 (assoc sn v true)]
                          (if (or (= (nth @mtR v) -1)
                                  (let [[ok sn3] (bpm (nth @mtR v) sn2)]
                                    (if ok true false)))
                            (do (swap! mtR assoc v u) [true sn2])
                            (recur (rest vs) sn2))))))))]
        (doseq [u (range n)]
          (bpm u (vec (repeat n false))))
        (let [mtL (reduce (fn [acc v] (if (= (nth @mtR v) -1) acc (assoc acc (nth @mtR v) v)))
                          (vec (repeat n -1)) (range n))
              paths (vec (sort-by first
                           (filterv some?
                             (mapv (fn [s] (when (= (nth @mtR s) -1)
                                              (loop [p [s]]
                                                (if (= (nth mtL (peek p)) -1) p (recur (conj p (nth mtL (peek p))))))))
                                   (range n)))))]
          (println (count paths))
          (doseq [p paths]
            (println (clojure.string/join " " p))))))))
