;; Sparse linear systems (NR 2.7): CCS matvecs + BiCG.
;; Port of the C++ reference.
(require '[clojure.string :as str])

(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        ip (long x)
        fp (long (+ (* (- x ip) 1000000.0) 0.5))
        [ip fp] (if (= fp 1000000) [(inc ip) 0] [ip fp])]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

(defn absv [v] (if (neg? v) (- v) v))

(defn canon [v] (if (< (absv v) 5e-7) 0.0 v))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          nnz (Long/parseLong (nth toks 2))
          raw (subvec toks 3)
          trips (vec (sort-by (fn [t] [(second t) (first t)])
                              (map (fn [k] [(Long/parseLong (nth raw (* k 3)))
                                            (Long/parseLong (nth raw (+ (* k 3) 1)))
                                            (Double/parseDouble (nth raw (+ (* k 3) 2)))])
                                   (range nnz))))
          col-ptr (loop [t 0 cp (vec (repeat (inc n) 0))]
                    (if (>= t nnz) cp
                        (recur (inc t) (update cp (inc (second (nth trips t))) inc))))
          col-ptr (loop [j 0 cp col-ptr]
                    (if (>= j n) cp
                        (recur (inc j) (update cp (inc j) + (nth cp j)))))
          [row-ind vals] (loop [t 0 nxt col-ptr ri (vec (repeat nnz 0)) vv (vec (repeat nnz 0.0))]
                           (if (>= t nnz) [ri vv]
                               (let [k (nth nxt (second (nth trips t)))]
                                 (recur (inc t)
                                        (update nxt (second (nth trips t)) inc)
                                        (assoc ri k (first (nth trips t)))
                                        (assoc vv k (nth (nth trips t) 2))))))
          ax (fn [x]
               (reduce (fn [y j]
                         (reduce (fn [y i] (update y (nth row-ind i) + (* (nth vals i) (nth x j))))
                                 y (range (nth col-ptr j) (nth col-ptr (inc j)))))
                       (vec (repeat n 0.0)) (range n)))
          atx (fn [x]
                (mapv (fn [i]
                        (reduce + 0.0 (map (fn [j] (* (nth vals j) (nth x (nth row-ind j))))
                                           (range (nth col-ptr i) (nth col-ptr (inc i))))))
                      (range n)))
          diag (mapv (fn [i]
                       (or (some (fn [j] (when (= (nth row-ind j) i) (nth vals j)))
                                 (range (nth col-ptr i) (nth col-ptr (inc i))))
                           0.0))
                     (range n))
          asolve (fn [b] (mapv (fn [i] (let [d (nth diag i)] (if (zero? d) (nth b i) (/ (nth b i) d)))) (range n)))
          dot (fn [a b] (reduce + 0.0 (map * a b)))
          rest-toks (subvec raw (* nnz 3))]
      (cond
        (or (= op "MATVEC") (= op "TMATVEC"))
        (let [x (mapv #(Double/parseDouble %) (subvec rest-toks 0 n))
              y ((if (= op "MATVEC") ax atx) x)]
          (print (str/join " " (map #(fmt6 (canon %)) y))))
        :else
        (let [b (mapv #(Double/parseDouble %) (subvec rest-toks 0 n))
              itmax (Long/parseLong (nth rest-toks n))
              ax0 (ax (vec (repeat n 0.0)))
              r0 (mapv - b ax0)
              [x rn] (loop [iter 1 x (vec (repeat n 0.0)) p (vec (repeat n 0.0))
                            pp (vec (repeat n 0.0)) r r0 rr r0 z (asolve r0) bkden 1.0]
                       (if (> iter itmax)
                         (let [axf (ax x)
                               rn (Math/sqrt (reduce + 0.0 (map (fn [j] (let [d (- (nth b j) (nth axf j))] (* d d))) (range n))))]
                           [x rn])
                         (let [zz (asolve rr)
                               bknum (dot z rr)
                               [p pp] (if (= iter 1) [z zz]
                                          (let [bk (if (zero? bkden) 0.0 (/ bknum bkden))]
                                            [(mapv (fn [j] (+ (* bk (nth p j)) (nth z j))) (range n))
                                             (mapv (fn [j] (+ (* bk (nth pp j)) (nth zz j))) (range n))]))
                               z (ax p)
                               akden (dot z pp)
                               ak (if (zero? akden) 0.0 (/ bknum akden))
                               zz (atx pp)]
                           (recur (inc iter)
                                  (mapv (fn [j] (+ (nth x j) (* ak (nth p j)))) (range n))
                                  p pp
                                  (mapv (fn [j] (- (nth r j) (* ak (nth z j)))) (range n))
                                  (mapv (fn [j] (- (nth rr j) (* ak (nth zz j)))) (range n))
                                  (asolve (mapv (fn [j] (- (nth r j) (* ak (nth z j)))) (range n)))
                                  bknum))))]
          (print (str (str/join " " (map #(fmt6 (canon %)) x)) "\n" (fmt6 (canon rn)))))))))
