;; fleury: Eulerian trail avoiding bridges
;;
;; Fleury construction: from the current end take the smallest
;; numbered non-bridge edge (bridge test by reachability count),
;; starting at an odd vertex when one exists. Emits the trail or a
;; no-trail marker.
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
          edges (mapv (fn [i] [(nth nums (+ 2 (* i 2))) (nth nums (+ 3 (* i 2)))]) (range m))
          adj (reduce (fn [acc i] (let [[u v] (nth edges i)]
                                     (-> acc (update u conj [v i]) (update v conj [u i]))))
                       (mapv (fn [_] []) (range n)) (range m))
          alive (atom (vec (repeat m true)))
          reach (fn [s]
                  (loop [st [s] sn (assoc (vec (repeat n false)) s true) c 0]
                    (if (empty? st) c
                      (let [u (peek st)]
                        (recur (reduce (fn [a [w e]] (if (and (nth @alive e) (not (nth sn w))) (conj a w) a))
                                       (pop st) (nth adj u))
                               (reduce (fn [a [w e]] (if (and (nth @alive e) (not (nth a w))) (assoc a w true) a))
                                       sn (nth adj u))
                               (inc c))))))]
      (let [odd (filterv #(odd? (count (nth adj %))) (range n))]
        (if (and (not= (count odd) 0) (not= (count odd) 2))
          (println "NO EULERIAN TRAIL")
          (let [src (first (filter #(seq (nth adj %)) (range n)))]
            (if (nil? src)
              (println 0)
              (let [sn (loop [st [src] sn (assoc (vec (repeat n false)) src true)]
                         (if (empty? st) sn
                           (let [u (peek st)]
                             (recur (reduce (fn [a [w _]] (if (nth sn w) a (conj a w))) (pop st) (nth adj u))
                                    (reduce (fn [a [w _]] (assoc a w true)) sn (nth adj u))))))]
                (if (some #(and (seq (nth adj %)) (not (nth sn %))) (range n))
                  (println "NO EULERIAN TRAIL")
                  (let [start (if (seq odd) (first odd) src)]
                    (loop [u start trail [start]]
                      (let [cands (vec (sort (filterv (fn [[_ e]] (nth @alive e)) (nth adj u))))]
                        (if (empty? cands)
                          (if (some true? @alive)
                            (println "NO EULERIAN TRAIL")
                            (println (clojure.string/join " " trail)))
                          (let [[w e] (if (= (count cands) 1)
                                        (first cands)
                                        (let [base (reach u)]
                                          (loop [cs cands]
                                            (let [[nw ne] (first cs)]
                                              (swap! alive assoc ne false)
                                              (if (>= (reach u) base)
                                                (do (swap! alive assoc ne true) [nw ne])
                                                (do (swap! alive assoc ne true)
                                                    (if (= (count cs) 1) [nw ne] (recur (rest cs)))))))))]
                            (swap! alive assoc e false)
                            (recur w (conj trail w))))))))))))))))
