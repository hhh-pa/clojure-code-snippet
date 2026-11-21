;; QR decomposition (NR 2.10, QRdcmp). Port of the C++ reference.
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

(defn sgn [a b] (if (neg? b) (- (absv a)) (absv a)))

(defn rotate [r qt n i aa bb]
  (let [[cc s] (cond (zero? aa) [0.0 (if (neg? bb) -1.0 1.0)]
                     (> (absv aa) (absv bb))
                     (let [fact (/ bb aa)
                           cc (sgn (/ 1.0 (Math/sqrt (+ 1.0 (* fact fact)))) aa)]
                       [cc (* fact cc)])
                     :else
                     (let [fact (/ aa bb)
                           s (sgn (/ 1.0 (Math/sqrt (+ 1.0 (* fact fact)))) bb)]
                       [(* fact s) s]))
        r (reduce (fn [r j]
                    (let [y (get-in r [i j]) w (get-in r [(inc i) j])]
                      (assoc-in (assoc-in r [i j] (- (* cc y) (* s w)))
                                [(inc i) j] (+ (* s y) (* cc w)))))
                  r (range i n))
        qt (reduce (fn [qt j]
                     (let [y (get-in qt [i j]) w (get-in qt [(inc i) j])]
                       (assoc-in (assoc-in qt [i j] (- (* cc y) (* s w)))
                                 [(inc i) j] (+ (* s y) (* cc w)))))
                   qt (range n))]
    [r qt]))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          nums (mapv #(Double/parseDouble %) (subvec toks 2))
          a (vec (map #(vec (subvec nums (* % n) (+ (* % n) n))) (range n)))
          restv (subvec nums (* n n))
          b (vec (subvec restv 0 (min n (count restv))))
          u (if (= op "UPDATE") (vec (subvec restv 0 n)) (vec (repeat n 0.0)))
          v (if (= op "UPDATE") (vec (subvec restv n (+ n n))) (vec (repeat n 0.0)))
          [r c d sing] (loop [k 0 r a c (vec (repeat n 0.0)) d (vec (repeat n 0.0)) sing false]
                         (if (>= k (dec n)) [r c d sing]
                             (let [scale (reduce #(max %1 (absv (get-in r [%2 k]))) 0.0 (range k n))]
                               (if (zero? scale)
                                 (recur (inc k) r (assoc c k 0.0) (assoc d k 0.0) true)
                                 (let [r (reduce (fn [r i] (assoc-in r [i k] (/ (get-in r [i k]) scale))) r (range k n))
                                       sum (reduce + 0.0 (map (fn [i] (let [x (get-in r [i k])] (* x x))) (range k n)))
                                       sigma (sgn (Math/sqrt sum) (get-in r [k k]))
                                       r (assoc-in r [k k] (+ (get-in r [k k]) sigma))
                                       c (assoc c k (* sigma (get-in r [k k])))
                                       d (assoc d k (* (- scale) sigma))
                                       r (reduce (fn [r j]
                                                   (let [s (reduce + 0.0 (map (fn [i] (* (get-in r [i k]) (get-in r [i j]))) (range k n)))
                                                         tau (/ s (nth c k))]
                                                     (reduce (fn [r i] (assoc-in r [i j] (- (get-in r [i j]) (* tau (get-in r [i k])))))
                                                             r (range k n))))
                                                 r (range (inc k) n))]
                                   (recur (inc k) r c d sing))))))
          d (assoc d (dec n) (get-in r [(dec n) (dec n)]))
          sing (or sing (zero? (nth d (dec n))))
          qt (vec (map (fn [i] (vec (map (fn [j] (if (= i j) 1.0 0.0)) (range n)))) (range n)))
          qt (loop [k 0 qt qt]
               (if (>= k (dec n)) qt
                   (recur (inc k)
                          (if (zero? (nth c k)) qt
                              (reduce (fn [qt j]
                                        (let [s (/ (reduce + 0.0 (map (fn [i] (* (get-in r [i k]) (get-in qt [i j]))) (range k n)))
                                                  (nth c k))]
                                          (reduce (fn [qt i] (assoc-in qt [i j] (- (get-in qt [i j]) (* s (get-in r [i k])))))
                                                  qt (range k n))))
                                      qt (range n))))))
          r (vec (map-indexed (fn [i row] (vec (map-indexed (fn [j x] (cond (= i j) (nth d i) (< j i) 0.0 :else x)) row))) r))
          [r qt sing] (if (= op "UPDATE")
                        (let [w u
                              k (loop [k (dec n)] (if (or (< k 0) (not (zero? (nth w k)))) k (recur (dec k))))
                              k (if (< k 0) 0 k)
                              [r qt w] (loop [i (dec k) r r qt qt w w]
                                         (if (< i 0) [r qt w]
                                             (let [[r qt] (rotate r qt n i (nth w i) (- (nth w (inc i))))
                                                   w (assoc w i (cond (zero? (nth w i)) (absv (nth w (inc i)))
                                                                       (> (absv (nth w i)) (absv (nth w (inc i))))
                                                                       (let [q (/ (nth w (inc i)) (nth w i))]
                                                                         (* (absv (nth w i)) (Math/sqrt (+ 1.0 (* q q)))))
                                                                       :else
                                                                       (let [q (/ (nth w i) (nth w (inc i)))]
                                                                         (* (absv (nth w (inc i))) (Math/sqrt (+ 1.0 (* q q)))))))]
                                               (recur (dec i) r qt w))))
                              r (reduce (fn [r i] (assoc-in r [0 i] (+ (get-in r [0 i]) (* (nth w 0) (nth v i))))) r (range n))
                              [r qt] (loop [i 0 r r qt qt]
                                       (if (>= i k) [r qt]
                                           (let [[r qt] (rotate r qt n i (get-in r [i i]) (- (get-in r [(inc i) i])))]
                                             (recur (inc i) r qt))))
                              sing (or sing (some #(zero? (get-in r [% %])) (range n)))]
                          [r qt sing])
                        [r qt sing])]
      (cond
        (or (= op "FACTOR") (= op "UPDATE"))
        (print (str "QT\n"
                    (str/join "\n" (map #(str/join " " (map (fn [x] (fmt6 (canon x))) %)) qt))
                    "\nR\n"
                    (str/join "\n" (map #(str/join " " (map (fn [x] (fmt6 (canon x))) %)) r))
                    "\nSINGULAR " (if sing 1 0)))
        sing (print "SINGULAR")
        :else
        (let [x (mapv (fn [i] (reduce + 0.0 (map (fn [j] (* (get-in qt [i j]) (nth b j))) (range n)))) (range n))
              x (loop [i (dec n) x x]
                  (if (< i 0) x
                      (recur (dec i) (assoc x i (/ (- (nth x i)
                                                       (reduce + 0.0 (map (fn [j] (* (get-in r [i j]) (nth x j)))
                                                                          (range (inc i) n))))
                                                    (get-in r [i i]))))))]
          (print (str/join " " (map #(fmt6 (canon %)) x))))))))
