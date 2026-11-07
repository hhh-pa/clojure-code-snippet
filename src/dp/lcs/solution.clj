(def digit-val
  "Map from decimal char to numeric value for pure parsing."
  (zipmap "0123456789" (range)))

(defn tokenize
  "Split string on whitespace into a vector of token strings using pure reduce."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[\x00-\x20]+"))))

(defn parse-num
  "Parse decimal token to integer value or nil when shape is bad."
  [tok]
  (if-let [v (when (string? tok) (parse-long tok))]
    v
    (let [cs (seq tok)]
      (when (seq cs)
        (let [head (first cs)
              neg (= head \-)
              plus (= head \+)
              rest-cs (if (or neg plus) (next cs) cs)]
          (when (seq rest-cs)
            (let [val (reduce (fn [acc c]
                                (let [d (get digit-val c)]
                                  (if (nil? d)
                                    (reduced nil)
                                    (+' (*' acc 10) d))))
                              0 rest-cs)]
              (when (some? val)
                (if neg (- 0 val) val)))))))))

(defn parse-all
  "Parse every token with parse-num, giving nil when any token is bad."
  [toks]
  (reduce (fn [acc t]
            (let [v (parse-num t)]
              (if (nil? v)
                (reduced nil)
                (conj acc v))))
          [] toks))

(defn lcs-scores
  "Row of longest match lengths for x against each prefix of y.
  Time O(mn) and space O(n) with two rows, treating row ops as unit cost."
  [x y]
  (let [nx (count x)
        m (count y)
        base (vec (repeat (inc m) 0))]
    (loop [i 0 prev base]
      (if (>= i nx)
        prev
        (let [xi (nth x i)
              cur (transient base)]
          (loop [j 1 left 0 diag 0]
            (when (<= j m)
              (let [up (nth prev j)
                    v (if (= xi (nth y (dec j)))
                        (inc diag)
                        (if (>= up left) up left))]
                (assoc! cur j v)
                (recur (inc j) v up))))
          (recur (inc i) (persistent! cur)))))))

(defn in-vec
  "True when v equals some entry of coll, pure linear scan."
  [coll v]
  (let [n (count coll)]
    (loop [i 0]
      (cond
        (>= i n) false
        (= (nth coll i) v) true
        :else (recur (inc i))))))

(defn rev-vec [v]
  (let [n (count v)
        out (transient [])]
    (loop [i (dec n)]
      (if (neg? i)
        (persistent! out)
        (do (conj! out (nth v i))
            (recur (dec i)))))))

(defn hirschberg
  "Longest shared run of x and y with linear extra rows and first split.
  Time O(mn) and extra space O(n), split choice mirrors full table backtrack."
  [x y]
  (let [nx (count x)
        ny (count y)]
    (cond
      (or (zero? nx) (zero? ny))
      []

      (= 1 nx)
      (let [v (nth x 0)]
        (if (in-vec y v) [v] []))

      (= 1 ny)
      (let [v (nth y 0)]
        (if (in-vec x v) [v] []))

      :else
      (let [mid (quot nx 2)
            x1 (subvec x 0 mid)
            x2 (subvec x mid)
            fwd (lcs-scores x1 y)
            back (lcs-scores (rev-vec x2) (rev-vec y))
            best-k (loop [k 0 best-val -1 best-k 0]
                     (if (> k ny)
                       best-k
                       (let [v (+ (nth fwd k) (nth back (- ny k)))]
                         (if (> v best-val)
                           (recur (inc k) v k)
                           (recur (inc k) best-val best-k)))))]
        (into (hirschberg x1 (subvec y 0 best-k))
              (hirschberg x2 (subvec y best-k)))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn -main
  "Read two int runs from stdin and print shared run, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [nums (parse-all toks)]
        (cond
          (nil? nums)
          (complain "invalid input: expected integers only")

          (< (count nums) 2)
          (complain "invalid input: expected two lengths first")

          :else
          (let [n (nth nums 0)
                m (nth nums 1)]
            (cond
              (or (< n 0) (< m 0))
              (complain "invalid input: lengths must be zero or more")

              (< (count nums) (+' 2 n m))
              (complain "invalid input: not enough run values")

              :else
              (let [a (subvec nums 2 (+ 2 n))
                    b (subvec nums (+ 2 n) (+ 2 n m))
                    ans (hirschberg b a)
                    line (clojure.string/join " " ans)]
                (println line)))))))))

(-main)
