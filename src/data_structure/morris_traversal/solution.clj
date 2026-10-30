(require 'clojure.string)

;; Morris Inorder & Preorder Binary Tree Traversal (Karumanchi Ch 6, CLRS Ch 12):
;; Threaded-predecessor traversal simulation in O(n) time and O(n) persistent
;; state in pure functional Clojure (corresponding to O(1) auxiliary space in
;; mutable pointer representations).

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

(defn valid-child?
  "True when child index c is either -1 or in [0, n)."
  [c n]
  (or (= c -1) (and (>= c 0) (< c n))))

(defn parse-nodes
  "Parse n node triples [val left right] from toks starting at index 1."
  [toks n]
  (when (= (count toks) (inc (* 3 n)))
    (loop [i 0 ti 1 vals [] lefts [] rights []]
      (if (= i n)
        {:val vals :left lefts :right rights}
        (let [v (parse-num (nth toks ti))
              l (parse-num (nth toks (+ ti 1)))
              r (parse-num (nth toks (+ ti 2)))]
          (if (or (nil? v) (nil? l) (nil? r)
                  (not (valid-child? l n))
                  (not (valid-child? r n)))
            nil
            (recur (inc i) (+ ti 3) (conj vals v) (conj lefts l) (conj rights r))))))))

(defn find-pred
  "Find the Morris predecessor of curr starting at (left curr)."
  [right l curr]
  (loop [p l]
    (let [rp (nth right p)]
      (if (and (not= rp -1) (not= rp curr))
        (recur rp)
        p))))

(defn morris-inorder
  "Perform Morris inorder traversal by threading and restoring right links."
  [vals lefts rights]
  (loop [curr 0 right rights out []]
    (if (= curr -1)
      out
      (let [l (nth lefts curr)]
        (if (= l -1)
          (recur (nth right curr) right (conj out (nth vals curr)))
          (let [pred (find-pred right l curr)
                rp (nth right pred)]
            (if (= rp -1)
              (recur l (assoc right pred curr) out)
              (let [right2 (assoc right pred -1)]
                (recur (nth right2 curr) right2 (conj out (nth vals curr)))))))))))

(defn morris-preorder
  "Perform Morris preorder traversal by threading and restoring right links."
  [vals lefts rights]
  (loop [curr 0 right rights out []]
    (if (= curr -1)
      out
      (let [l (nth lefts curr)]
        (if (= l -1)
          (recur (nth right curr) right (conj out (nth vals curr)))
          (let [pred (find-pred right l curr)
                rp (nth right pred)]
            (if (= rp -1)
              (recur l (assoc right pred curr) (conj out (nth vals curr)))
              (let [right2 (assoc right pred -1)]
                (recur (nth right2 curr) right2 out)))))))))

(defn -main
  "Parse the binary tree and emit Morris inorder and preorder traversals."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (<= n 0))
      (fail "invalid input: expected positive node count N")
      (let [tree (parse-nodes toks n)]
        (if (nil? tree)
          (fail "invalid input: expected N valid [val left right] triples")
          (let [in-res (morris-inorder (:val tree) (:left tree) (:right tree))
                pre-res (morris-preorder (:val tree) (:left tree) (:right tree))]
            (emit-lines [(clojure.string/join " " (map str in-res))
                         (clojure.string/join " " (map str pre-res))])))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
