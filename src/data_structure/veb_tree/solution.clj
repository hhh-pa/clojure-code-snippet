(require 'clojure.string)

;; van Emde Boas Tree (CLRS Ch 20):
;; Supports Insert, Delete, Member, Successor, Predecessor in O(log log U) time
;; and Min/Max in O(1) time over universe [0, U).

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

(defn log2-pow2
  "Return k such that 2^k >= u."
  [u]
  (loop [k 0 p 1]
    (if (>= p u)
      k
      (recur (inc k) (* p 2)))))

(defn pow2
  "Compute 2^k."
  [k]
  (loop [i 0 acc 1]
    (if (= i k)
      acc
      (recur (inc i) (* acc 2)))))

(defn make-veb
  "Create an empty vEB tree node for universe size u."
  [u]
  (let [k (log2-pow2 u)
        lk (quot k 2)
        uk (- k lk)
        l-sqrt (pow2 lk)
        u-sqrt (pow2 uk)]
    {:u u
     :l-sqrt l-sqrt
     :u-sqrt u-sqrt
     :min -1
     :max -1
     :summary nil
     :cluster {}}))

(defn veb-min
  "Return minimum element of vEB tree node v, or -1 if empty."
  [v]
  (if (nil? v) -1 (:min v)))

(defn veb-max
  "Return maximum element of vEB tree node v, or -1 if empty."
  [v]
  (if (nil? v) -1 (:max v)))

(defn veb-high
  "High cluster index of x in v."
  [v x]
  (quot x (:l-sqrt v)))

(defn veb-low
  "Low offset within cluster of x in v."
  [v x]
  (rem x (:l-sqrt v)))

(defn veb-index
  "Reconstruct key from cluster index h and offset l."
  [v h l]
  (+ (* h (:l-sqrt v)) l))

(defn get-summary
  "Return summary vEB node of v, initializing if nil."
  [v]
  (or (:summary v) (make-veb (:u-sqrt v))))

(defn get-cluster
  "Return cluster h of v, initializing if nil."
  [v h]
  (or (get (:cluster v) h) (make-veb (:l-sqrt v))))

(defn veb-empty-insert
  "Insert x into an empty vEB node v."
  [v x]
  (assoc v :min x :max x))

(defn veb-member?
  "Check whether x is present in vEB tree v."
  [v x]
  (cond
    (nil? v) false
    (or (< x 0) (>= x (:u v))) false
    (or (= x (:min v)) (= x (:max v))) true
    (= (:u v) 2) false
    :else (veb-member? (get (:cluster v) (veb-high v x)) (veb-low v x))))

(defn veb-insert
  "Insert x into vEB tree v (ignoring duplicates)."
  [v x]
  (cond
    (or (< x 0) (>= x (:u v))) v
    (= (:min v) -1) (veb-empty-insert v x)
    (or (= x (:min v)) (= x (:max v))) v
    :else
    (let [[v1 x1] (if (< x (:min v))
                    [(assoc v :min x) (:min v)]
                    [v x])
          v2 (if (> (:u v1) 2)
               (let [h (veb-high v1 x1)
                     l (veb-low v1 x1)
                     c (get-cluster v1 h)]
                 (if (= (veb-min c) -1)
                   (let [sum' (veb-insert (get-summary v1) h)
                         c' (veb-empty-insert c l)]
                     (assoc v1 :summary sum' :cluster (assoc (:cluster v1) h c')))
                   (let [c' (veb-insert c l)]
                     (assoc v1 :cluster (assoc (:cluster v1) h c')))))
               v1)]
      (if (> x1 (:max v2))
        (assoc v2 :max x1)
        v2))))

(defn veb-delete-present
  "Delete x when x is guaranteed to be a member of v."
  [v x]
  (cond
    (= (:min v) (:max v))
    (assoc v :min -1 :max -1)

    (= (:u v) 2)
    (let [m (if (= x 0) 1 0)]
      (assoc v :min m :max m))

    :else
    (let [[v1 x1] (if (= x (:min v))
                    (let [fc (veb-min (:summary v))
                          new-min (veb-index v fc (veb-min (get (:cluster v) fc)))]
                      [(assoc v :min new-min) new-min])
                    [v x])
          h (veb-high v1 x1)
          l (veb-low v1 x1)
          c' (veb-delete-present (get-cluster v1 h) l)
          clusters' (assoc (:cluster v1) h c')]
      (if (= (veb-min c') -1)
        (let [sum' (veb-delete-present (get-summary v1) h)
              v2 (assoc v1 :summary sum' :cluster clusters')]
          (if (= x1 (:max v2))
            (let [smax (veb-max sum')]
              (if (= smax -1)
                (assoc v2 :max (:min v2))
                (let [cmax (veb-max (get clusters' smax))]
                  (assoc v2 :max (veb-index v2 smax cmax)))))
            v2))
        (let [v2 (assoc v1 :cluster clusters')]
          (if (= x1 (:max v2))
            (assoc v2 :max (veb-index v2 h (veb-max c')))
            v2))))))

(defn veb-delete
  "Delete x from vEB tree v if present."
  [v x]
  (if (veb-member? v x)
    (veb-delete-present v x)
    v))

(defn veb-successor
  "Return smallest key > x in vEB tree v, or -1 if none."
  [v x]
  (cond
    (or (nil? v) (= (:min v) -1) (>= x (:max v))) -1
    (< x (:min v)) (:min v)
    (= (:u v) 2) (if (and (= x 0) (= (:max v) 1)) 1 -1)
    :else
    (let [h (veb-high v x)
          l (veb-low v x)
          c (get (:cluster v) h)
          max-low (veb-max c)]
      (if (and (not= max-low -1) (< l max-low))
        (veb-index v h (veb-successor c l))
        (let [succ-c (veb-successor (:summary v) h)]
          (if (= succ-c -1)
            -1
            (veb-index v succ-c (veb-min (get (:cluster v) succ-c)))))))))

(defn veb-predecessor
  "Return largest key < x in vEB tree v, or -1 if none."
  [v x]
  (cond
    (or (nil? v) (= (:min v) -1) (<= x (:min v))) -1
    (> x (:max v)) (:max v)
    (= (:u v) 2) (if (and (= x 1) (= (:min v) 0)) 0 -1)
    :else
    (let [h (veb-high v x)
          l (veb-low v x)
          c (get (:cluster v) h)
          min-low (veb-min c)]
      (if (and (not= min-low -1) (> l min-low))
        (veb-index v h (veb-predecessor c l))
        (let [pred-c (veb-predecessor (:summary v) h)]
          (if (= pred-c -1)
            (if (and (not= (:min v) -1) (> x (:min v)))
              (:min v)
              -1)
            (veb-index v pred-c (veb-max (get (:cluster v) pred-c)))))))))

(defn run-ops
  "Execute q vEB tree operations from tokens."
  [toks u q]
  (let [nt (count toks)]
    (loop [i 0 ti 2 tree (make-veb u) out []]
      (if (= i q)
        out
        (if (>= ti nt)
          nil
          (let [op (parse-num (nth toks ti))]
            (cond
              (or (= op 1) (= op 2) (= op 3) (= op 4) (= op 5))
              (if (>= (inc ti) nt)
                nil
                (let [x (parse-num (nth toks (inc ti)))]
                  (if (nil? x)
                    nil
                    (case op
                      1 (recur (inc i) (+ ti 2) (veb-insert tree x) out)
                      2 (recur (inc i) (+ ti 2) (veb-delete tree x) out)
                      3 (recur (inc i) (+ ti 2) tree (conj out (if (veb-member? tree x) "1" "0")))
                      4 (recur (inc i) (+ ti 2) tree (conj out (str (veb-successor tree x))))
                      5 (recur (inc i) (+ ti 2) tree (conj out (str (veb-predecessor tree x))))))))

              (= op 6)
              (recur (inc i) (inc ti) tree
                     (conj out (str (veb-min tree) " " (veb-max tree))))

              :else nil)))))))

(defn -main
  "Parse universe size u and operation count q, then run vEB operations."
  [input]
  (let [toks (tokenize input)]
    (if (< (count toks) 2)
      (fail "invalid input: expected U and Q")
      (let [u (parse-num (nth toks 0))
            q (parse-num (nth toks 1))]
        (if (or (nil? u) (nil? q) (< u 2) (> u 65536) (neg? q))
          (fail "invalid U or Q for veb_tree")
          (let [lines (run-ops toks u q)]
            (if (nil? lines)
              (fail "invalid operations for veb_tree")
              (emit-lines lines))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
