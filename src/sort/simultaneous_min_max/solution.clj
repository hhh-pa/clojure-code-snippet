;; simultaneous-min-max: pairwise tournament for min and max in
;; 3*floor(n/2) comparisons (CLRS 9.1). On even n the first pair
;; seeds min and max; on odd n the first element seeds both. Each
;; subsequent pair contributes one comparison to order the pair plus
;; one each toward the running min and max. O(n) time, O(1) extra
;; space. Pure Clojure: loop and recur over the input vector.
;; Input: n followed by n values on stdin; output: "min max\\n".

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

(defn simultaneous-min-max
  "Pairwise tournament: 3*floor(n/2) comparisons. Returns [mn mx]."
  [xs]
  (let [n (count xs)]
    (cond
      (zero? n) [0 0]
      (= n 1) [(first xs) (first xs)]
      :else
      (let [even? (even? n)
            init-pair (if even?
                        (let [a (nth xs 0) b (nth xs 1)]
                          (if (< a b) [a b] [b a]))
                        [(nth xs 0) (nth xs 0)])
            start (if even? 2 1)]
        (loop [i start mn (first init-pair) mx (second init-pair)]
          (if (>= (inc i) n)
            [mn mx]
            (let [x (nth xs i) y (nth xs (inc i))
                  sm (min x y) lg (max x y)]
              (recur (+ i 2) (min mn sm) (max mx lg)))))))))

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected integers"))
          (let [n (first nums)
                vals (vec (take (min n (max 0 (dec (count nums)))) (rest nums)))]
            (if (empty? vals)
              (println "0 0")
              (let [[mn mx] (simultaneous-min-max vals)]
                (println mn mx)))))))))

(-main (slurp *in*))
