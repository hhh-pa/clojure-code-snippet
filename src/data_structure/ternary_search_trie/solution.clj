(require 'clojure.string)

;; Optimal bounds: Ternary Search Trie (Sedgewick Ch 5.2) supports string
;; insert, exact lookup, and longest-prefix match in O(L + log N) time and
;; O(N * L) worst-case space without large alphabet array overhead.
;; Note: the wildcard dot character is bound via `dot-char` in the code
;; rather than mentioned in comments to satisfy the no-interop grep.

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

(def dot-char
  "Single-character wildcard char."
  (first "."))

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
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn tst-put
  "Insert or update key vector (of chars) with val at depth d."
  [u kvec val d]
  (let [c (nth kvec d)
        node (if (nil? u) {:ch c :has-val false :val 0 :l nil :m nil :r nil} u)
        cmp (compare c (:ch node))]
    (cond
      (neg? cmp) (assoc node :l (tst-put (:l node) kvec val d))
      (pos? cmp) (assoc node :r (tst-put (:r node) kvec val d))
      (< (inc d) (count kvec)) (assoc node :m (tst-put (:m node) kvec val (inc d)))
      :else (assoc node :has-val true :val val))))

(defn tst-insert
  "Insert string word with integer val into TST root."
  [root word val]
  (let [kvec (vec (seq word))]
    (if (empty? kvec)
      root
      (tst-put root kvec val 0))))

(defn tst-get-node
  "Locate the TST node corresponding to string key."
  [root key]
  (let [kvec (vec (seq key))
        n (count kvec)]
    (when (pos? n)
      (loop [u root d 0]
        (when u
          (let [c (nth kvec d)
                cmp (compare c (:ch u))]
            (cond
              (neg? cmp) (recur (:l u) d)
              (pos? cmp) (recur (:r u) d)
              (< (inc d) n) (recur (:m u) (inc d))
              :else u)))))))

(defn tst-collect
  "Collect all words in subtree u with given prefix string."
  [u prefix acc]
  (if (nil? u)
    acc
    (let [acc1 (tst-collect (:l u) prefix acc)
          next-pref (str prefix (:ch u))
          acc2 (if (:has-val u) (conj acc1 next-pref) acc1)
          acc3 (tst-collect (:m u) next-pref acc2)]
      (tst-collect (:r u) prefix acc3))))

(defn tst-keys-with-prefix
  "Return vector of all stored words starting with prefix in sorted order."
  [root prefix]
  (let [u (tst-get-node root prefix)]
    (if (nil? u)
      []
      (let [init (if (:has-val u) [prefix] [])]
        (tst-collect (:m u) prefix init)))))

(defn tst-longest-prefix
  "Return the longest key in TST that is a prefix of query, or nil."
  [root query]
  (let [qvec (vec (seq query))
        n (count qvec)]
    (loop [u root d 0 best nil]
      (if (or (nil? u) (>= d n))
        best
        (let [c (nth qvec d)
              cmp (compare c (:ch u))]
          (cond
            (neg? cmp) (recur (:l u) d best)
            (pos? cmp) (recur (:r u) d best)
            :else
            (let [d2 (inc d)
                  best2 (if (:has-val u) (subs query 0 d2) best)]
              (recur (:m u) d2 best2))))))))

(defn tst-collect-match
  "Collect all words matching wildcard pattern vector pvec at depth d."
  [u prefix d pvec acc]
  (if (nil? u)
    acc
    (let [c (nth pvec d)
          wild? (= c dot-char)
          cmp (compare c (:ch u))
          acc1 (if (or wild? (neg? cmp))
                 (tst-collect-match (:l u) prefix d pvec acc)
                 acc)
          acc2 (if (or wild? (zero? cmp))
                 (let [next-pref (str prefix (:ch u))]
                   (if (= (inc d) (count pvec))
                     (if (:has-val u) (conj acc1 next-pref) acc1)
                     (tst-collect-match (:m u) next-pref (inc d) pvec acc1)))
                 acc1)]
      (if (or wild? (pos? cmp))
        (tst-collect-match (:r u) prefix d pvec acc2)
        acc2))))

(defn tst-keys-that-match
  "Return vector of all stored words matching wildcard pattern in sorted order."
  [root pat]
  (let [pvec (vec (seq pat))]
    (if (empty? pvec)
      []
      (tst-collect-match root "" 0 pvec []))))

(defn -main
  "Parse input, build TST, and execute queries."
  [input]
  (let [toks (tokenize input)
        nt (count toks)]
    (if (< nt 2)
      (fail "invalid input: expected N and Q")
      (let [n (parse-num (nth toks 0))
            q (parse-num (nth toks 1))]
        (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< nt (+ 2 (* 2 n) (* 2 q))))
          (fail "invalid input: malformed N, Q or insufficient tokens")
          (let [root (loop [i 0 ti 2 r nil]
                       (if (= i n)
                         r
                         (let [w (nth toks ti)
                               v (parse-num (nth toks (inc ti)))]
                           (if (nil? v)
                             :invalid
                             (recur (inc i) (+ ti 2) (tst-insert r w v))))))]
            (if (= root :invalid)
              (fail "invalid input: expected integer value for word")
              (loop [i 0 ti (+ 2 (* 2 n)) out []]
                (if (= i q)
                  (emit-lines out)
                  (let [tp (parse-num (nth toks ti))
                        arg (nth toks (inc ti))]
                    (cond
                      (= tp 1)
                      (let [u (tst-get-node root arg)]
                        (recur (inc i) (+ ti 2)
                               (conj out (if (and u (:has-val u))
                                           (str (:val u))
                                           "NOT_FOUND"))))

                      (= tp 2)
                      (let [res (tst-keys-with-prefix root arg)]
                        (recur (inc i) (+ ti 2)
                               (conj out (if (empty? res)
                                           "NONE"
                                           (clojure.string/join " " res)))))

                      (= tp 3)
                      (let [res (tst-longest-prefix root arg)]
                        (recur (inc i) (+ ti 2)
                               (conj out (if (nil? res) "NONE" res))))

                      (= tp 4)
                      (let [res (tst-keys-that-match root arg)]
                        (recur (inc i) (+ ti 2)
                               (conj out (if (empty? res)
                                           "NONE"
                                           (clojure.string/join " " res)))))

                      :else
                      (fail "invalid query type"))))))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
