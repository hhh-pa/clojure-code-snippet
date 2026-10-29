(require 'clojure.string)

;; Optimal bounds: building the search tree costs O(n h) for height h, and the
;; lowest common ancestor walk costs O(h), which is optimal for this problem.

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

(defn bst-insert
  "Insert v into the search tree, sending duplicates right."
  [tree v]
  (if (nil? tree)
    {:val v :left nil :right nil}
    (if (< v (:val tree))
      (assoc tree :left (bst-insert (:left tree) v))
      (assoc tree :right (bst-insert (:right tree) v)))))

(defn lowest-common-ancestor
  "Walk down from root toward the split point of lo and hi."
  [root lo hi]
  (loop [curr root]
    (when curr
      (cond
        (< hi (:val curr)) (recur (:left curr))
        (> lo (:val curr)) (recur (:right curr))
        :else (:val curr)))))

(defn -main
  "Read n values plus two query values, then print their ancestor."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (neg? n) (< (count toks) (+' n 3)))
      (fail "invalid input: expected a count n, then n values, then two more")
      (let [vals (mapv parse-num (take n (rest toks)))
            v1 (parse-num (nth toks (inc n)))
            v2 (parse-num (nth toks (+ 2 n)))]
        (if (or (some nil? vals) (nil? v1) (nil? v2))
          (fail "invalid input: expected a count n, then n values, then two more")
          (let [root (reduce bst-insert nil vals)
                found (lowest-common-ancestor root (min v1 v2) (max v1 v2))]
            (when found
              (println found))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
