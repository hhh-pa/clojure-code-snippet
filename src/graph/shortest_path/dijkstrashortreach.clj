;; Dijkstra shortest reach over many cases in pure clojure. A
;; binary heap over a flat transient vector (distance and vertex
;; alternating) gives O(E log V) time and O(V + E) space per case,
;; the best known practical bound for non negative weights; stale
;; entries are skipped on the way out. The heap, the parser, and
;; the adjacency build keep every counter primitive and reuse
;; boxed indices, so the per-edge work allocates O(1) small boxes
;; only on relaxations and nothing on skips. Output keeps the old
;; shape with minus one for unreachable verts and one line per case.
;; Input shape: case count t, then per case n m, m lines x y r with
;; 1 based verts, and a source. Blank stdin prints nothing. Badly
;; shaped input prints one message to stderr and returns with exit
;; 0, since a nonzero exit needs host interop, avoided by design.

(defn- parse-ints
  "Reads all signed integers from string s into a vector, else nil.
   One scalar loop: the accumulator is a primitive long, so the
   per-char work is inlined primops and allocates nothing. Digits
   past signed 64-bit range fail the scan. Negated numbers
   accumulate negatively (never negated at flush, which would throw
   on exactly -2^63 since unary minus is not inlined); the
   boundary digit is 8 then, 7 otherwise."
  [s]
  (loop [i (long 0) nn (long (count s)) nums (transient [])
         cur (long 0) started false neg false bad false]
    (if (>= i nn)
      (when-not bad
        (cond
          started (persistent! (conj! nums cur))
          neg nil
          :else (persistent! nums)))
      (if bad
        (recur (+ i 1) nn nums cur started neg bad)
        (let [code (int (nth s i))]
          (cond
            (and (<= 48 code) (<= code 57))
            (let [d (- code 48)]
              (if (if neg
                    (or (< cur -922337203685477580)
                        (and (== cur -922337203685477580) (> d 8)))
                    (or (> cur 922337203685477580)
                        (and (== cur 922337203685477580) (> d 7))))
                (recur (+ i 1) nn nums cur started neg true)
                (recur (+ i 1) nn nums
                       ;; (long d): d is let-boxed; unboxing at the
                       ;; use site keeps cur primitive every step.
                       (if neg (- (* cur 10) (long d)) (+ (* cur 10) (long d)))
                       true neg bad)))
            (== code 45)
            (if (or started neg)
              (recur (+ i 1) nn nums cur started neg true)
              (recur (+ i 1) nn nums cur started true bad))
            (<= code 32)
            (if started
              (recur (+ i 1) nn (conj! nums cur) 0 false false bad)
              (if neg
                (recur (+ i 1) nn nums cur started neg true)
                (recur (+ i 1) nn nums cur started neg bad)))
            :else
            (recur (+ i 1) nn nums cur started neg true)))))))

(defn- report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn- ensure-idx
  "Grows the boxed-index vector in idxbox until it holds need
   slots. Push sites call this first; with non-negative weights
   the initial size always suffices and the probe allocates
   nothing."
  [idxbox need]
  (let [idx @idxbox]
    ;; Probing with a not-found value avoids a count box; only a
    ;; genuine shortfall grows (and boxes), doubling to need.
    (when (nil? (nth idx (long need) nil))
      (swap! idxbox (fn [v] (let [c (count v)]
                              (persistent! (reduce conj! (transient v)
                                                   (range c (max (* 2 c) (long need)))))))))))

(defn- sift-up
  "Moves the entry at e up the flat transient heap hp (distance
   and vertex alternating) until ordered; idxbox holds boxed
   indices so assoc! keys reuse existing objects. Returns hp. The
   hole formulation writes the entry once; all counters primitive."
  [hp idxbox e]
  ;; e arrives boxed (one box per call); every use unboxes it at
  ;; the use site, which is free, so the loop stays primitive.
  (let [idx @idxbox
        ed (nth hp (* 2 (long e))) ev (nth hp (+ (* 2 (long e)) 1))]
    (loop [h (long e) p (long (bit-shift-right (- (long e) 1) 1))]
      (if (<= h 0)
        (do (assoc! hp (nth idx (* 2 h)) ed)
            (assoc! hp (nth idx (+ (* 2 h) 1)) ev)
            hp)
        (if (>= (long ed) (long (nth hp (* 2 p))))
          (do (assoc! hp (nth idx (* 2 h)) ed)
              (assoc! hp (nth idx (+ (* 2 h) 1)) ev)
              hp)
          (do (assoc! hp (nth idx (* 2 h)) (nth hp (* 2 p)))
              (assoc! hp (nth idx (+ (* 2 h) 1)) (nth hp (+ (* 2 p) 1)))
              (recur p (bit-shift-right (- p 1) 1))))))))

(defn- sift-down
  "Moves the entry at 0 down the flat heap hp with bound n
   entries; idxbox holds boxed indices. Returns hp. The winning
   child is selected by duplicating the two branches rather than
   binding it, since a let would box once per level."
  [hp idxbox n]
  (let [idx @idxbox
        ed (nth hp 0) ev (nth hp 1)]
    (loop [h (long 0) l (long 1) nn (long n)]
      (if (>= l nn)
        (do (assoc! hp (nth idx (* 2 h)) ed)
            (assoc! hp (nth idx (+ (* 2 h) 1)) ev)
            hp)
        (if (and (< (+ l 1) nn)
                 (< (long (nth hp (* 2 (+ l 1)))) (long (nth hp (* 2 l)))))
          (if (>= (long (nth hp (* 2 (+ l 1)))) (long ed))
            (do (assoc! hp (nth idx (* 2 h)) ed)
                (assoc! hp (nth idx (+ (* 2 h) 1)) ev)
                hp)
            (do (assoc! hp (nth idx (* 2 h)) (nth hp (* 2 (+ l 1))))
                (assoc! hp (nth idx (+ (* 2 h) 1)) (nth hp (+ (* 2 (+ l 1)) 1)))
                (recur (+ l 1) (+ (* 2 (+ l 1)) 1) nn)))
          (if (>= (long (nth hp (* 2 l))) (long ed))
            (do (assoc! hp (nth idx (* 2 h)) ed)
                (assoc! hp (nth idx (+ (* 2 h) 1)) ev)
                hp)
            (do (assoc! hp (nth idx (* 2 h)) (nth hp (* 2 l)))
                (assoc! hp (nth idx (+ (* 2 h) 1)) (nth hp (+ (* 2 l) 1)))
                (recur l (+ (* 2 l) 1) nn))))))))

(defn- add-exact
  "Exact sum of the boxed numbers cd and w: primitive arithmetic
   with an overflow guard, falling back to +' past 64-bit range.
   Called once per first visit, so its boxes are O(V)."
  [cd w maxl minl]
  (if (or (> cd maxl) (< cd minl))
    (+' cd w)
    (let [a (long cd) b (long w)]
      (if (or (and (>= b 0) (> a (- (long maxl) b)))
              (and (< b 0) (< a (- (long minl) b))))
        (+' cd w)
        (+ a b)))))

(defn- dijkstra-line
  "Runs one case from token position p and returns the pair of next
   position and output line, or nil when the shape is wrong.
   Adjacency is flat neighbor/weight pairs in transient vectors;
   the heap shares one boxed-index vector with the build so keyed
   writes reuse existing objects. Skipped edges allocate nothing."
  [nums p]
  (let [c (count nums)]
    (when (>= (- c p) 2)
      (let [n (nth nums p)
            m (nth nums (inc p))]
        (when (and (>= n 1) (>= m 0) (>= (- c p) (+' 3 (*' m 3))))
          (let [idxbox (atom (vec (range (max (* 2 n) (* 4 (+ m 1))))))
                ;; Boxed indices shared by every keyed write, grown
                ;; on demand by ensure-idx. Heap positions stay
                ;; below 4m+4 with non-negative weights; negative
                ;; weights may push more and grow the vector.
                idx0 @idxbox
                built (loop [j (long 0) nn (long n)
                             a (transient (vec (repeat n [])))]
                        (if (>= j nn)
                          a
                          (recur (+ j 1) nn
                                 (assoc! a (nth idx0 j) (transient [])))))
                built (loop [i (long 0) pp (long p) mm (long m) a built]
                        (if (>= i mm)
                          a
                          (let [x (nth nums (+ pp 2 (* i 3)))
                                y (nth nums (+ pp 3 (* i 3)))
                                r (nth nums (+ pp 4 (* i 3)))]
                            (if (or (< x 1) (> x n) (< y 1) (> y n))
                              nil
                              (let [ax (nth idx0 (- (long x) 1))
                                    bx (nth idx0 (- (long y) 1))]
                                (recur (+ i 1) pp mm
                                       (-> a
                                           (assoc! ax (conj! (conj! (nth a ax) bx) r))
                                           (assoc! bx (conj! (conj! (nth a bx) ax) r)))))))))]
            (when (some? built)
              (let [adj (loop [j (long 0) nn (long n) a built]
                          (if (>= j nn)
                            (persistent! a)
                            (let [jx (nth idx0 j)]
                              (recur (+ j 1) nn
                                     (assoc! a jx (persistent! (nth a jx)))))))
                    src (nth nums (+ p 2 (* m 3)))]
                (when (and (>= src 1) (<= src n))
                  (let [s (dec src)
                        maxl 9223372036854775807
                        minl -9223372036854775808
                        dist (assoc! (transient (vec (repeat n -1))) s 0)
                        hp0 (conj! (conj! (transient []) 0) s)
                        dist (loop [hp hp0 sz (long 1) dist dist]
                               (if (<= sz 0)
                                 dist
                                 (let [cd (nth hp 0)
                                       v (nth hp 1)
                                       idx @idxbox
                                       hp (if (> sz 1)
                                            (let [ld (nth hp (* (- sz 1) 2))
                                                  lv (nth hp (+ (* (- sz 1) 2) 1))]
                                              (assoc! hp (nth idx 0) ld)
                                              (assoc! hp (nth idx 1) lv)
                                              (sift-down hp idxbox (- sz 1)))
                                            hp)]
                                   (if (not (= cd (nth dist v)))
                                     (recur hp (- sz 1) dist)
                                     (let [es (nth adj v)
                                           sz1 (loop [k (long 0)
                                                      lim (long (count es))
                                                      sz (long (- sz 1))
                                                      hi (long maxl)
                                                      lo (long minl)]
                                                 (if (>= k lim)
                                                   sz
                                                   (let [to (nth es k)
                                                         w (nth es (+ k 1))
                                                         old (nth dist to)]
                                                     (cond
                                                       (= old -1)
                                                       (let [ndv (add-exact cd w maxl minl)]
                                                         (assoc! dist to ndv)
                                                         ;; conj! grows capacity at the tail; the
                                                         ;; entry is copied to 2sz since pops
                                                         ;; never shrink the transient.
                                                         (ensure-idx idxbox (+ (* 2 sz) 2))
                                                         (let [idx @idxbox]
                                                           (conj! hp ndv) (conj! hp to)
                                                           (assoc! hp (nth idx (* 2 sz)) ndv)
                                                           (assoc! hp (nth idx (+ (* 2 sz) 1)) to)
                                                           (sift-up hp idxbox sz))
                                                         (recur (+ k 2) lim (+ sz 1) hi lo))
                                                       (or (> cd maxl) (< cd minl)
                                                           (> old maxl) (< old minl))
                                                       (let [ndv (+' cd w)]
                                                         (if (< ndv old)
                                                           (do (assoc! dist to ndv)
                                                               (ensure-idx idxbox (+ (* 2 sz) 2))
                                                               (let [idx @idxbox]
                                                                 (conj! hp ndv) (conj! hp to)
                                                                 (assoc! hp (nth idx (* 2 sz)) ndv)
                                                                 (assoc! hp (nth idx (+ (* 2 sz) 1)) to)
                                                                 (sift-up hp idxbox sz))
                                                               (recur (+ k 2) lim (+ sz 1) hi lo))
                                                           (recur (+ k 2) lim sz hi lo)))
                                                       :else
                                                       (let [cdl (long cd) wl (long w)]
                                                         (if (or (and (>= wl 0) (> cdl (- hi wl)))
                                                                 (and (< wl 0) (< cdl (- lo wl))))
                                                           (let [ndv (+' cd w)]
                                                             (if (< ndv old)
                                                               (do (assoc! dist to ndv)
                                                                   (ensure-idx idxbox (+ (* 2 sz) 2))
                                                                   (let [idx @idxbox]
                                                                     (conj! hp ndv) (conj! hp to)
                                                                     (assoc! hp (nth idx (* 2 sz)) ndv)
                                                                     (assoc! hp (nth idx (+ (* 2 sz) 1)) to)
                                                                     (sift-up hp idxbox sz))
                                                                   (recur (+ k 2) lim (+ sz 1) hi lo))
                                                               (recur (+ k 2) lim sz hi lo)))
                                                           (let [ndv (+ cdl wl)]
                                                             (if (< ndv (long old))
                                                               (let [ndvb (long ndv)]
                                                                 (assoc! dist to ndvb)
                                                                 (ensure-idx idxbox (+ (* 2 sz) 2))
                                                                 (let [idx @idxbox]
                                                                   (conj! hp ndvb) (conj! hp to)
                                                                   (assoc! hp (nth idx (* 2 sz)) ndvb)
                                                                   (assoc! hp (nth idx (+ (* 2 sz) 1)) to)
                                                                   (sift-up hp idxbox sz))
                                                                 (recur (+ k 2) lim (+ sz 1) hi lo))
                                                               (recur (+ k 2) lim sz hi lo)))))))))]
                                       (recur hp sz1 dist))))))]
                    [(+ p 3 (* m 3))
                     (str (apply str (mapv (fn [i] (str (nth dist i) " "))
                                           (filterv (fn [i] (not (= i s))) (range n))))
                          "\n")]))))))))))

(defn- solve
  "Solves all cases from the token vector and prints the answers."
  [nums]
  (let [t (nth nums 0)]
    (if (< t 0)
      (report-error "bad input: case count must not be negative")
      (if (zero? t)
        nil
        (let [lines (loop [tc 0 p 1 acc []]
                      (if (>= tc t)
                        acc
                        (let [one (dijkstra-line nums p)]
                          (if (nil? one)
                            nil
                            (recur (inc tc) (first one)
                                   (conj acc (second one)))))))]
          (if (nil? lines)
            (report-error "bad input: case shape wrong or vertex out of range")
            (do (print (apply str lines)) nil)))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
