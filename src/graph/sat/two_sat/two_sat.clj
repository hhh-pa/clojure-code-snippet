;; two-sat: 2-SAT by implication SCCs
;;
;; Tarjan SCCs over the 2n-node implication graph; a variable
;; sharing its negation component is unsatisfiable, else later
;; components set truth. Emits the verdict and an assignment.
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
    (let [n (nth nums 0) m (nth nums 1) N (* 2 n)
          lit (fn [k] (let [v (dec (if (neg? k) (- k) k))] (if (pos? k) (inc (* 2 v)) (* 2 v))))
          adj (atom (mapv (fn [_] []) (range N)))]
      (doseq [i (range m)]
        (let [a (nth nums (+ 2 (* i 2))) b (nth nums (+ 3 (* i 2)))]
          (swap! adj update (lit (- a)) conj (lit b))
          (swap! adj update (lit (- b)) conj (lit a))))
      (let [idx (atom (vec (repeat N 0))) low (atom (vec (repeat N 0)))
            on (atom (vec (repeat N false))) st (atom [])
            comp (atom (vec (repeat N -1))) C (atom 0) T (atom 0)]
        (letfn [(dfs [v]
                  (swap! T inc) (swap! idx assoc v @T) (swap! low assoc v @T)
                  (swap! st conj v) (swap! on assoc v true)
                  (doseq [w (nth @adj v)]
                    (cond (= (nth @idx w) 0)
                          (do (dfs w) (swap! low assoc v (min (nth @low v) (nth @low w))))
                          (nth @on w)
                          (swap! low assoc v (min (nth @low v) (nth @idx w)))))
                  (when (= (nth @low v) (nth @idx v))
                    (loop []
                      (let [x (peek @st)]
                        (swap! st pop) (swap! on assoc x false) (swap! comp assoc x @C)
                        (when (not= x v) (recur))))
                    (swap! C inc)))]
          (doseq [v (range N)]
            (when (zero? (nth @idx v)) (dfs v)))
          (if (some #(= (nth @comp (* 2 %)) (nth @comp (inc (* 2 %)))) (range n))
            (println "UNSATISFIABLE")
            (do (println "SATISFIABLE")
                (println (clojure.string/join " " (map #(if (> (nth @comp (* 2 %)) (nth @comp (inc (* 2 %)))) 1 0) (range n)))))))))))
