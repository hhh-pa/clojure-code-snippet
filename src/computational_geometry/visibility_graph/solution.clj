;; visibility-graph: visibility graph shortest path among
;; polygonal obstacles. Exact integer visibility (proper crossings
;; plus vertex pass-through midpoint tests), O(V^2) Dijkstra with
;; (dist,index) order, length printed %.6f (US locale).
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
                             {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))
(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))
(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))
(defn orient [a b c]
  (- (* (- (nth b 0) (nth a 0)) (- (nth c 1) (nth a 1)))
     (* (- (nth b 1) (nth a 1)) (- (nth c 0) (nth a 0)))))
(defn proper-cross [a b c d]
  (let [o1 (orient a b c) o2 (orient a b d) o3 (orient c d a) o4 (orient c d b)]
    (and (not= (pos? o1) (pos? o2)) (not (zero? o1)) (not (zero? o2))
         (not= (pos? o3) (pos? o4)) (not (zero? o3)) (not (zero? o4)))))
(defn on-open [p a b]
  (and (zero? (orient a b p))
       (neg? (+ (* (- (nth p 0) (nth a 0)) (- (nth p 0) (nth b 0)))
                (* (- (nth p 1) (nth a 1)) (- (nth p 1) (nth b 1)))))))
(defn on-closed2 [px py a b]
  (let [ax (* 2 (nth a 0)) ay (* 2 (nth a 1)) bx (* 2 (nth b 0)) by (* 2 (nth b 1))]
    (and (zero? (- (* (- bx ax) (- py ay)) (* (- by ay) (- px ax))))
         (<= (min ax bx) px (max ax bx)) (<= (min ay by) py (max ay by)))))
(defn pip-strict [X Y P]
  (let [m (count P)]
    (when-not (some (fn [i] (on-closed2 X Y (nth P i) (nth P (mod (inc i) m)))) (range m))
      (loop [i 0 inside false]
        (if (>= i m) inside
          (let [cx (* 2 (nth (nth P i) 0)) cy (* 2 (nth (nth P i) 1))
                dx (* 2 (nth (nth P (mod (inc i) m)) 0)) dy (* 2 (nth (nth P (mod (inc i) m)) 1))]
            (recur (inc i) (if (not= (> cy Y) (> dy Y))
                             (let [cond (if (> dy cy)
                                          (< (* X (- dy cy)) (+ (* cx (- dy cy)) (* (- dx cx) (- Y cy))))
                                          (> (* X (- dy cy)) (+ (* cx (- dy cy)) (* (- dx cx) (- Y cy)))))]
                               (if cond (not inside) inside))
                             inside))))))))
(defn visible [a b obs]
  (every? (fn [P]
            (let [m (count P)]
              (and (not-any? (fn [i] (proper-cross a b (nth P i) (nth P (mod (inc i) m)))) (range m))
                   (every? (fn [v]
                             (or (not (on-open v a b))
                                 (and (not (pip-strict (+ (nth a 0) (nth v 0)) (+ (nth a 1) (nth v 1)) P))
                                      (not (pip-strict (+ (nth v 0) (nth b 0)) (+ (nth v 1) (nth b 1)) P))))) P)))) obs))
(defn ew [a b]
  (let [dx (double (- (nth a 0) (nth b 0))) dy (double (- (nth a 1) (nth b 1)))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        [obs p] (loop [i 0 p 1 obs []]
                  (if (>= i n) [obs p]
                    (let [k (nth nums p)
                          poly (vec (for [j (range k)] [(nth nums (+ p 1 (* 2 j))) (nth nums (+ p 2 (* 2 j)))]))]
                      (recur (inc i) (+ p 1 (* 2 k)) (conj obs poly)))))
        s [(nth nums p) (nth nums (inc p))]
        g [(nth nums (+ p 2)) (nth nums (+ p 3))]
        pts (vec (concat [s g] (apply concat obs)))
        V (count pts)
        edges (vec (for [i (range V) j (range (inc i) V) :when (visible (nth pts i) (nth pts j) obs)] [i j]))
        wmat (let [m (vec (repeat V (vec (repeat V -1.0))))]
               (reduce (fn [m [i j]] (let [w (ew (nth pts i) (nth pts j))] (assoc (assoc m i (assoc (nth m i) j w)) j (assoc (nth m j) i w)))) m edges))
        out (java.lang.StringBuilder.)]
    (.append out (str (count edges) "\n"))
    (doseq [[u v] edges] (.append out (str u " " v "\n")))
    (let [INF Double/POSITIVE_INFINITY
          [dist prev] (loop [it 0 dist (assoc (vec (repeat V INF)) 0 0.0) prev (vec (repeat V -1)) used (vec (repeat V false))]
                        (if (>= it V) [dist prev]
                          (let [u (reduce (fn [u i] (if (nth used i) u (if (or (= u -1) (< (nth dist i) (nth dist u))) i u))) -1 (range V))]
                            (if (or (= u -1) (= (nth dist u) INF)) [dist prev]
                              (if (= u 1) [dist prev]
                                (let [used2 (assoc used u true)
                                      [dist2 prev2] (loop [v 0 d dist pr prev]
                                                      (if (>= v V) [d pr]
                                                        (let [w (get-in wmat [u v])]
                                                          (recur (inc v)
                                                                 (if (and (>= w 0.0) (not (nth used2 v)) (< (+ (nth d u) w) (nth d v)))
                                                                   (assoc d v (+ (nth d u) w)) d)
                                                                 (if (and (>= w 0.0) (not (nth used2 v)) (< (+ (nth d u) w) (nth d v)))
                                                                   (assoc pr v u) pr)))))]
                                  (recur (inc it) dist2 prev2 used2)))))))]
      (if (= (nth dist 1) INF)
        (.append out "0\n\n-1\n")
        (let [path (loop [c 1 acc []] (if (= c -1) acc (recur (nth prev c) (conj acc c))))]
          (.append out (str (count path) "\n"))
          (.append out (str (clojure.string/join " " (reverse path)) "\n"))
          (.append out (str (String/format java.util.Locale/US "%.6f" (into-array Object [(nth dist 1)])) "\n"))))
      (print (str out)))))
(-main)
