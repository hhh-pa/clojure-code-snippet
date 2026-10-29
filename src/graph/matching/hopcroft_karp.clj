;; Hopcroft Karp maximum bipartite matching in pure clojure.
;; Layered breadth first searches plus depth first searches over
;; the layered graph give O(E sqrt V) time and O(E) space, the best
;; known practical bound for this problem. The depth first search
;; uses an explicit stack of edge positions, so long augmenting
;; paths cannot overflow the call stack, while edge order and pair
;; choice match the old run exactly.
;; Input shape: n m e, then e lines u v with 1 based left and right
;; verts. Output is the matching size plus one u v line per matched
;; left vertex in increasing u order. Blank stdin prints nothing.
;; Badly shaped input prints one message to stderr and returns with
;; exit 0, since a nonzero exit needs host interop, avoided by design.

(require 'clojure.string)

(def digit-values
  "Maps each digit char to its numeric value for the tokenizer below."
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


(defn parse-ints
  "Reads all signed integers from string s into a vector. Chars are
   tested with core compare fns after int coercion, digits through
   digit-values, and anything else fails the whole scan with nil."
  [s]
  (let [done
        (reduce
         (fn [state ch]
           (let [nums (:nums state)
                 cur (:cur state)
                 neg (:neg state)
                 bad (:bad state)
                 code (int ch)]
             (cond
               bad state
               (<= (int \0) code (int \9))
               (assoc state :cur (+' (*' (or cur 0) 10) (digit-values ch)))
               (= ch \-)
               (if (or (some? cur) neg)
                 (assoc state :bad true)
                 (assoc state :neg true))
               (<= code (int \space))
               (cond
                 (some? cur)
                 (let [v (fit-long (if neg (-' cur) cur))]
                   (if (some? v)
                     (-> state
                     (assoc :nums (conj! nums v))
                     (assoc :cur nil)
                     (assoc :neg false))
                     (assoc state :bad true)))
                 neg (assoc state :bad true)
                 :else state)
               :else (assoc state :bad true))))
         {:nums (transient []) :cur nil :neg false :bad false}
         (seq s))]
    (when-not (:bad done)
      (let [nums (:nums done)
            cur (:cur done)
            neg (:neg done)]
        (cond
          (some? cur) (let [v (fit-long (if neg (-' cur) cur))]
                        (when (some? v) (persistent! (conj! nums v))))
          neg nil
          :else (persistent! nums))))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn hk-bfs
  "Builds one layering over free left verts in input u order and
   scans each adjacency list in input edge order. Returns the pair
   of dist and true when the free nil vertex is reached."
  [adj pair-u pair-v dist n inf]
  (let [[queue dist] (loop [u 1 queue (transient []) dist dist]
                       (if (> u n)
                         [(persistent! queue) dist]
                         (if (zero? (nth pair-u u))
                           (recur (inc u)
                                  (conj! queue u)
                                  (assoc! dist u 0))
                           (recur (inc u) queue (assoc! dist u inf)))))
        dist (assoc! dist 0 inf)
        dist (loop [head 0 queue queue dist dist]
               (if (>= head (count queue))
                 dist
                 (let [u (nth queue head)]
                   (if (< (nth dist u) (nth dist 0))
                     (let [[queue dist]
                           (loop [es (nth adj u) queue queue dist dist]
                             (if (empty? es)
                               [queue dist]
                               (let [v (first es)
                                     w (nth pair-v v)]
                                 (if (= (nth dist w) inf)
                                   (recur (rest es)
                                          (conj queue w)
                                          (assoc! dist w
                                                  (inc (nth dist u))))
                                   (recur (rest es) queue dist)))))]
                       (recur (inc head) queue dist))
                     (recur (inc head) queue dist)))))]
    [dist (not= (nth dist 0) inf)]))

(defn hk-dfs
  "Searches one augmenting path from free vertex start through the
   layered graph. Frames hold [u next edge index], edges scan in
   input order, and success augments the whole frame stack, exactly
   like the classic recursive walk. Returns the tuple of dist and
   pair maps plus true on success."
  [adj pair-u pair-v dist start]
  (loop [stack [[start 0]] pair-u pair-u pair-v pair-v dist dist]
    (if (empty? stack)
      [dist pair-u pair-v false]
      (let [[u idx] (peek stack)
            nbrs (nth adj u)]
        (if (>= idx (count nbrs))
          (recur (pop stack) pair-u pair-v (assoc! dist u 1000000000))
          (let [v (nth nbrs idx)
                w (nth pair-v v)
                stack (conj (pop stack) [u (inc idx)])]
            (if (= (nth dist w) (inc (nth dist u)))
              (if (zero? w)
                (let [[pair-u pair-v]
                      (loop [frames stack pair-u pair-u pair-v pair-v]
                        (if (empty? frames)
                          [pair-u pair-v]
                          (let [[fu fidx] (peek frames)
                                fv (nth (nth adj fu) (dec fidx))]
                            (recur (pop frames)
                                   (assoc! pair-u fu fv)
                                   (assoc! pair-v fv fu)))))]
                  [dist pair-u pair-v true])
                (recur (conj stack [w 0]) pair-u pair-v dist))
              (recur stack pair-u pair-v dist))))))))

(defn max-matching
  "Runs layered phases until no augmenting path is left. Returns the
   pair of pair-u and the matching size."
  [adj n m]
  (let [pair-u (transient (vec (repeat (inc n) 0)))
        pair-v (transient (vec (repeat (inc m) 0)))
        dist (transient (vec (repeat (inc n) 0)))
        inf 1000000000]
    (loop [dist dist pair-u pair-u pair-v pair-v matching 0]
      (let [[dist found] (hk-bfs adj pair-u pair-v dist n inf)]
        (if (not found)
          [(persistent! pair-u) matching]
          (let [[dist pair-u pair-v matching]
                (loop [u 1 dist dist pair-u pair-u pair-v pair-v
                       matching matching]
                  (if (> u n)
                    [dist pair-u pair-v matching]
                    (if (zero? (nth pair-u u))
                      (let [[dist pair-u pair-v ok]
                            (hk-dfs adj pair-u pair-v dist u)]
                        (recur (inc u) dist pair-u pair-v
                               (if ok (inc matching) matching)))
                      (recur (inc u) dist pair-u pair-v matching))))]
            (recur dist pair-u pair-v matching)))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)
        e (nth nums 2 nil)]
    (if (or (< c 3) (< n 0) (< m 0) (< e 0) (< c (+' 3 (*' e 2))))
      (report-error "bad input: want n m e plus e edge pairs")
      (let [adj (loop [i 0 adj (transient (vec (repeat (inc n) [])))]
                  (if (>= i e)
                    (persistent! adj)
                    (let [u (nth nums (+ 3 (* i 2)))
                          v (nth nums (+ 4 (* i 2)))]
                      (if (or (< u 1) (> u n) (< v 1) (> v m))
                        nil
                        (recur (inc i)
                               (assoc! adj u
                                       (conj (nth adj u) v)))))))]
        (if (nil? adj)
          (report-error "bad input: edge vertex out of range")
          (if (or (zero? n) (zero? m))
            (do (println 0) nil)
            (let [[pair-u size] (max-matching adj n m)
                  lines (transient [(str size)])]
              (loop [u 1 lines lines]
                (if (> u n)
                  (do (println (clojure.string/join "\n" (persistent! lines)))
                      nil)
                  (recur (inc u)
                         (if (zero? (nth pair-u u))
                           lines
                           (conj! lines (str u " " (nth pair-u u))))))))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
