(require 'clojure.string)

;; Optimal bounds: lazy propagation answers each range add and range sum in
;; O(log n) time after an O(n) build, using O(n) space for the two trees.

(def digit-values
  "Map from each digit char to its numeric value."
  (zipmap "0123456789" (range)))

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn fit-long
  "Returns n when it fits in a signed 64-bit long, else nil."
  [n]
  (when (<= min-long n max-long) n))


(def whitespace-chars
  "Chars treated as token separators on input."
  #{\space \tab \newline \return})

(defn tokenize
  "Split s into whitespace separated tokens in O(n) time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (contains? whitespace-chars ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-num
  "Parse tok as an integer, or nil when it is not one."
  [tok]
  (when (and (string? tok) (seq tok))
    (let [cs (seq tok)
          neg? (= (first cs) \-)
          ds (if neg? (next cs) cs)]
      (when (and (seq ds) (every? digit-values ds))
        (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch))) 0 ds)]
          (fit-long (if neg? (-' v) v)))))))

(defn fail
  "Report invalid input on stderr and return normally.
   Note: the process still exits 0 since a nonzero status needs host calls."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn build-tree
  "Fill tree nodes for interval [l r] from a, returning the new tree."
  [tree a node l r]
  (if (= l r)
    (assoc tree node (nth a l))
    (let [m (+ l (quot (- r l) 2))
          left (* 2 node)
          right (inc left)
          tree (build-tree tree a left l m)
          tree (build-tree tree a right (inc m) r)]
      (assoc tree node (+' (nth tree left) (nth tree right))))))

(defn push-down
  "Move the pending add at node into its children, returning [tree lazy]."
  [tree lazy node l r]
  (let [v (nth lazy node)]
    (if (zero? v)
      [tree lazy]
      (let [m (+ l (quot (- r l) 2))
            left (* 2 node)
            right (inc left)]
        [(-> tree
             (assoc left (+' (nth tree left) (*' v (+ 1 (- m l)))))
             (assoc right (+' (nth tree right) (*' v (- r m)))))
         (-> lazy
             (assoc left (+' (nth lazy left) v))
             (assoc right (+' (nth lazy right) v))
             (assoc node 0))]))))

(defn update-range
  "Add val over [ql qr], returning the new [tree lazy] pair."
  [tree lazy node l r ql qr val]
  (if (and (<= ql l) (<= r qr))
    [(assoc tree node (+' (nth tree node) (*' val (+ 1 (- r l)))))
     (assoc lazy node (+' (nth lazy node) val))]
    (let [[tree lazy] (push-down tree lazy node l r)
          m (+ l (quot (- r l) 2))
          left (* 2 node)
          right (inc left)
          [tree lazy] (if (<= ql m)
                        (update-range tree lazy left l m ql qr val)
                        [tree lazy])
          [tree lazy] (if (> qr m)
                        (update-range tree lazy right (inc m) r ql qr val)
                        [tree lazy])]
      [(assoc tree node (+' (nth tree left) (nth tree right))) lazy])))

(defn query-range
  "Sum over [ql qr] within [l r], carrying ancestor adds in carry."
  [tree lazy node l r ql qr carry]
  (let [own (+ (nth tree node) (* carry (+ 1 (- r l))))]
    (if (and (<= ql l) (<= r qr))
      own
      (let [down (+ carry (nth lazy node))
            m (+ l (quot (- r l) 2))
            s1 (if (<= ql m)
                 (query-range tree lazy (* 2 node) l m ql qr down)
                 0)
            s2 (if (> qr m)
                 (query-range tree lazy (inc (* 2 node)) (inc m) r ql qr down)
                 0)]
        (+ s1 s2)))))

(defn run-ops
  "Apply q ops from toks at ti, returning lines or nil when invalid."
  [toks q ti tree lazy n]
  (let [nt (count toks)]
    (loop [i 0 ti ti tree tree lazy lazy out []]
      (if (or (>= i q) (>= ti nt))
        out
        (let [op (nth toks ti)]
          (cond
            (= op "UPDATE")
            (if (>= (+ ti 3) nt)
              nil
              (let [l (parse-num (nth toks (+ ti 1)))
                    r (parse-num (nth toks (+ ti 2)))
                    val (parse-num (nth toks (+ ti 3)))]
                (if (or (nil? l) (nil? r) (nil? val)
                         (< l 1) (> r n) (> l r))
                  nil
                  (let [[tree lazy] (update-range tree lazy 1 0 (dec n)
                                                  (dec l) (dec r) val)]
                    (recur (inc i) (+ ti 4) tree lazy out)))))

            (= op "QUERY")
            (if (>= (+ ti 2) nt)
              nil
              (let [l (parse-num (nth toks (+ ti 1)))
                    r (parse-num (nth toks (+ ti 2)))]
                (if (or (nil? l) (nil? r) (< l 1) (> r n) (> l r))
                  nil
                  (recur (inc i) (+ ti 3) tree lazy
                         (conj out (str (query-range tree lazy 1 0 (dec n)
                                                     (dec l) (dec r) 0)))))))

            :else
            (recur (inc i) (inc ti) tree lazy out)))))))

(defn -main
  "Build the sum tree, then serve range adds and range sums."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))
        q (parse-num (second toks))]
    (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< (count toks) (+' 2 n)))
      (fail "invalid input: expected n and q, then n values, then q ops")
      (let [a (mapv parse-num (take n (drop 2 toks)))]
        (if (some nil? a)
          (fail "invalid input: expected n and q, then n values, then q ops")
          (let [tree (if (pos? n)
                       (build-tree (vec (repeat (* 4 n) 0)) a 1 0 (dec n))
                       (vec (repeat 4 0)))
                lazy (vec (repeat (* 4 (max 1 n)) 0))
                lines (run-ops toks q (+ 2 n) tree lazy n)]
            (if (nil? lines)
              (fail "invalid input: ranges must sit within 1 and n")
              (emit-lines lines))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
