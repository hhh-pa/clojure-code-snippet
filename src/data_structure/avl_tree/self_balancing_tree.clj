(require 'clojure.string)

;; Optimal bounds: each insert costs O(log n) time, the tree holds O(n) nodes.
;; Rotations keep every balance factor in [-1 1], so the height stays logarithmic.

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

(defn node-height
  "Height of node, with nil standing for height -1."
  [node]
  (if node (:ht node) -1))

(defn refresh-height
  "Return node with its cached height recomputed from its children."
  [node]
  (assoc node :ht (inc (max (node-height (:left node)) (node-height (:right node))))))

(defn balance-factor
  "Left height minus right height, or 0 for nil."
  [node]
  (if node (- (node-height (:left node)) (node-height (:right node))) 0))

(defn rotate-right
  "Single right rotation around y, returning the new subtree root."
  [y]
  (let [x (:left y)
        moved (:right x)]
    (-> x
        (assoc :right (refresh-height (assoc y :left moved)))
        (refresh-height))))

(defn rotate-left
  "Single left rotation around x, returning the new subtree root."
  [x]
  (let [y (:right x)
        moved (:left y)]
    (-> y
        (assoc :left (refresh-height (assoc x :right moved)))
        (refresh-height))))

(defn avl-insert
  "Insert val into the AVL tree rooted at node. Duplicates go right."
  [node val]
  (if (nil? node)
    {:val val :left nil :right nil :ht 0}
    (let [placed (if (< val (:val node))
                   (assoc node :left (avl-insert (:left node) val))
                   (assoc node :right (avl-insert (:right node) val)))
          grown (refresh-height placed)
          b (balance-factor grown)]
      (cond
        (> b 1)
        (let [fixed (if (> val (:val (:left grown)))
                      (assoc grown :left (rotate-left (:left grown)))
                      grown)]
          (rotate-right fixed))
        (< b -1)
        (let [fixed (if (< val (:val (:right grown)))
                      (assoc grown :right (rotate-right (:right grown)))
                      grown)]
          (rotate-left fixed))
        :else grown))))

(defn inorder-nodes
  "Nodes of the tree in symmetric order."
  [node]
  (when node
    (concat (inorder-nodes (:left node)) [node] (inorder-nodes (:right node)))))

(defn preorder-nodes
  "Nodes of the tree in prefix order."
  [node]
  (when node
    (concat [node] (preorder-nodes (:left node)) (preorder-nodes (:right node)))))

(defn show-node
  "Render one node exactly as val(BF=b) plus a trailing blank."
  [node]
  (str (:val node) "(BF=" (balance-factor node) ") "))

(defn -main
  "Read n, then n values, then one extra value; print inorder and preorder lines."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (neg? n) (< (count toks) (+' n 2)))
      (fail "invalid input: expected a count n, then n values, then one value")
      (let [vals (mapv parse-num (take n (rest toks)))
            extra (parse-num (nth toks (inc n)))]
        (if (or (some nil? vals) (nil? extra))
          (fail "invalid input: expected a count n, then n values, then one value")
          (let [root (reduce avl-insert nil (conj vals extra))]
            (println (apply str (map show-node (inorder-nodes root))))
            (println (apply str (map show-node (preorder-nodes root))))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
