;; Kosaraju strongly connected components in pure clojure. One
;; iterative depth first search over the reversed graph builds
;; postorder in O(V + E) time, then forward searches from that
;; order count component sizes in O(V + E) time, for O(V + E) time
;; and space total, the best known bound for this problem. Frames
;; keep child positions, exactly like genuine recursion, so deep
;; graphs cannot overflow the call stack. Output is the odd sized
;; total minus the even sized total.
;; Input shape: vertex count, one ignored number, then edge pairs
;; a b with 1 based verts until end of input, out of range pairs
;; skipped. Blank stdin prints nothing. Non integer chars make the
;; input bad, printing one message to stderr and returning with
;; exit 0, since a nonzero exit needs host interop, avoided here.

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
  "Reads all signed integers from string s into a vector. Tokens
   split on chars at or below space; a token that is not an optional
   minus and ASCII digits within long range fails the whole scan with
   nil."
  [s]
  (if (nil? s)
    []
    (reduce (fn [acc t]
              (if-let [v (when (re-matches #"-?[0-9]+" t) (parse-long t))]
                (conj acc (if (= v -9223372036854775808) (bigint v) v))
                (reduced nil)))
            []
            (filterv seq (clojure.string/split s #"[\x00-\x20]+")))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn post-order
  "Postorder of rg with an explicit frame stack, where each frame
   holds [vertex next child index], mirroring true recursion. Only
   starts from verts the caller has not visited yet. Returns the
   pair of finish order and visited table."
  [rg n visited order]
  (loop [v 0 visited visited order order]
    (if (>= v n)
      [order visited]
      (if (nth visited v)
        (recur (inc v) visited order)
        (let [[visited order]
              (loop [stack [[v 0]]
                     visited (assoc! visited v true)
                     order order]
                (if (empty? stack)
                  [visited order]
                  (let [[u idx] (peek stack)
                        nbrs (nth rg u)]
                    (if (< idx (count nbrs))
                      (let [w (nth nbrs idx)
                            stack (conj (pop stack) [u (inc idx)])]
                        (if (nth visited w)
                          (recur stack visited order)
                          (recur (conj stack [w 0])
                                 (assoc! visited w true)
                                 order)))
                      (recur (pop stack) visited (conj! order u))))))]
          (recur (inc v) visited order))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)]
    (if (< c 2)
      (report-error "bad input: want at least vertex count and filler")
      (let [n (nth nums 0)]
        (if (< n 0)
          (report-error "bad input: vertex count must not be negative")
          (let [npairs (quot (- c 2) 2)
                [g rg] (loop [k 0
                              g (transient (vec (repeat n [])))
                              rg (transient (vec (repeat n [])))]
                         (if (>= k npairs)
                           [(persistent! g) (persistent! rg)]
                           (let [a (dec (nth nums (+ 2 (* k 2))))
                                 b (dec (nth nums (+ 3 (* k 2))))]
                             (if (or (< a 0) (>= a n) (< b 0) (>= b n))
                               (recur (inc k) g rg)
                               (recur (inc k)
                                      (assoc! g a (conj (nth g a) b))
                                      (assoc! rg b (conj (nth rg b) a)))))))
                [order visited]
                (post-order rg n
                            (transient (vec (repeat n false)))
                            (transient []))
                visited (transient (vec (repeat n false)))
                diff (loop [idx (dec (count order))
                            visited visited
                            diff 0]
                       (if (< idx 0)
                         diff
                         (let [v (nth order idx)]
                           (if (nth visited v)
                             (recur (dec idx) visited diff)
                             (let [[visited size]
                                   (loop [stack [v]
                                          visited (assoc! visited v true)
                                          size 0]
                                     (if (empty? stack)
                                       [visited size]
                                       (let [x (peek stack)
                                             stack (pop stack)
                                             size (inc size)
                                             [stack visited]
                                             (loop [es (nth g x)
                                                    stack stack
                                                    visited visited]
                                               (if (empty? es)
                                                 [stack visited]
                                                 (let [w (first es)]
                                                   (if (nth visited w)
                                                     (recur (rest es)
                                                            stack visited)
                                                     (recur (rest es)
                                                            (conj stack w)
                                                            (assoc! visited
                                                                    w true))))))]
                                         (recur stack visited size))))]
                               (recur (dec idx) visited
                                      (if (odd? size)
                                        (+ diff size)
                                        (- diff size))))))))]
            (println diff)
            nil))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
