(require 'clojure.string)

;; Optimal bounds: Compressed Radix / Patricia Trie (Sedgewick Ch 5, CLRS Ch 12)
;; stores compressed edge labels so there are O(N) nodes for N keys, supporting
;; insert, delete, lookup, and prefix count in O(L) time per operation.

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
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn make-node
  "Create a radix trie node with edge string."
  [edge]
  {:edge edge :has-val false :val 0 :cnt 0 :children {}})

(defn update-cnt
  "Recompute subtree key count for node u."
  [u]
  (let [base (if (:has-val u) 1 0)
        sum-ch (reduce (fn [acc [_ ch]] (+ acc (:cnt ch))) 0 (:children u))]
    (assoc u :cnt (+ base sum-ch))))

(defn lcp-len
  "Return the length of the longest common prefix of strings a and b."
  [a b]
  (let [n (min (count a) (count b))]
    (loop [i 0]
      (if (and (< i n) (= (nth a i) (nth b i)))
        (recur (inc i))
        i))))

(defn radix-insert
  "Insert or update string rem with val in subtree u."
  [u rem val]
  (if (empty? rem)
    (update-cnt (assoc u :has-val true :val val))
    (let [c (nth rem 0)
          v (get (:children u) c)]
      (if (nil? v)
        (let [leaf {:edge rem :has-val true :val val :cnt 1 :children {}}]
          (update-cnt (assoc-in u [:children c] leaf)))
        (let [v-edge (:edge v)
              k (lcp-len rem v-edge)]
          (if (= k (count v-edge))
            (let [v2 (radix-insert v (subs rem k) val)]
              (update-cnt (assoc-in u [:children c] v2)))
            (let [mid-edge (subs v-edge 0 k)
                  v-rest (subs v-edge k)
                  v-shifted (assoc v :edge v-rest)
                  mid-base (assoc-in (make-node mid-edge) [:children (nth v-rest 0)] v-shifted)
                  mid-full (if (= k (count rem))
                             (assoc mid-base :has-val true :val val)
                             (let [leaf-edge (subs rem k)
                                   leaf {:edge leaf-edge :has-val true :val val :cnt 1 :children {}}]
                               (assoc-in mid-base [:children (nth leaf-edge 0)] leaf)))]
              (update-cnt (assoc-in u [:children c] (update-cnt mid-full))))))))))

(defn radix-erase
  "Delete string rem from subtree u, merging single-child non-terminal nodes."
  [u rem]
  (if (empty? rem)
    (if (:has-val u)
      (update-cnt (assoc u :has-val false))
      u)
    (let [c (nth rem 0)
          v (get (:children u) c)]
      (if (nil? v)
        u
        (let [v-edge (:edge v)
              k (lcp-len rem v-edge)]
          (if (< k (count v-edge))
            u
            (let [v2 (radix-erase v (subs rem k))
                  ch-map (:children v2)
                  ch-cnt (count ch-map)]
              (cond
                (and (not (:has-val v2)) (zero? ch-cnt))
                (update-cnt (update u :children dissoc c))

                (and (not (:has-val v2)) (= ch-cnt 1))
                (let [[_ gc] (first ch-map)
                      merged (assoc gc :edge (str (:edge v2) (:edge gc)))]
                  (update-cnt (assoc-in u [:children c] merged)))

                :else
                (update-cnt (assoc-in u [:children c] v2))))))))))

(defn radix-search
  "Look up word in radix tree u, returning val or nil."
  [u word]
  (loop [cur u rem word]
    (if (empty? rem)
      (when (:has-val cur) (:val cur))
      (let [v (get (:children cur) (nth rem 0))]
        (when v
          (let [v-edge (:edge v)
                k (lcp-len rem v-edge)]
            (when (= k (count v-edge))
              (recur v (subs rem k)))))))))

(defn radix-count-prefix
  "Count stored words starting with prefix in radix tree u."
  [u prefix]
  (loop [cur u rem prefix]
    (if (empty? rem)
      (:cnt cur)
      (let [v (get (:children cur) (nth rem 0))]
        (if (nil? v)
          0
          (let [v-edge (:edge v)
                k (lcp-len rem v-edge)]
            (cond
              (= k (count rem)) (:cnt v)
              (< k (count v-edge)) 0
              :else (recur v (subs rem k)))))))))

(defn -main
  "Execute q radix trie operations from input."
  [input]
  (let [toks (tokenize input)
        nt (count toks)]
    (if (zero? nt)
      (fail "invalid input: expected operation count Q")
      (let [q (parse-num (nth toks 0))]
        (if (or (nil? q) (neg? q))
          (fail "invalid input: Q must be a non-negative integer")
          (loop [i 0 ti 1 root (make-node "") out []]
            (if (= i q)
              (emit-lines out)
              (if (>= (inc ti) nt)
                (fail "invalid input: unexpected end of tokens")
                (let [tp (parse-num (nth toks ti))
                      w (nth toks (inc ti))]
                  (cond
                    (= tp 1)
                    (if (>= (+ ti 2) nt)
                      (fail "invalid input: missing value for insert")
                      (let [v (parse-num (nth toks (+ ti 2)))]
                        (if (nil? v)
                          (fail "invalid input: value must be integer")
                          (recur (inc i) (+ ti 3) (radix-insert root w v) out))))

                    (= tp 2)
                    (recur (inc i) (+ ti 2) (radix-erase root w) out)

                    (= tp 3)
                    (let [res (radix-search root w)]
                      (recur (inc i) (+ ti 2) root
                             (conj out (if (nil? res) "NOT_FOUND" (str res)))))

                    (= tp 4)
                    (recur (inc i) (+ ti 2) root
                           (conj out (str (radix-count-prefix root w))))

                    :else
                    (fail "invalid operation type")))))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
