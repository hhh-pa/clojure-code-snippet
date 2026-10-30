;; cartesian-rmq: Cartesian tree + Euler tour + sparse table (Bender-Farach).
;; O(n) build, O(1) range-min queries. Input: n; n values; q; q ranges
;; l r (0-based inclusive). Output min value per query. Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(declare cart-euler)

(defn build-cartesian [a n]
  (loop [i 0 left (vec (repeat n -1)) right (vec (repeat n -1))
         parent (vec (repeat n -1)) stack []]
    (if (>= i n) [left right parent]
      (let [[last2 st2] (loop [last -1 st stack]
                          (if (and (seq st) (> (nth a (peek st)) (nth a i)))
                            (recur (peek st) (pop st)) [last st]))
            right2 (if (seq st2) (assoc right (peek st2) i) right)
            parent2 (if (seq st2) (assoc parent i (peek st2)) parent)
            left2 (assoc left i last2)
            parent3 (if (not= last2 -1) (assoc parent2 last2 i) parent2)]
        (recur (+ i 1) left2 right2 parent3 (conj st2 i))))))

(defn cart-euler [left right u d tour depth first]
  (let [tour2 (conj tour u) depth2 (conj depth d) first2 (assoc first u (count tour))
        [tour3 depth3 first3] (if (= (nth left u) -1) [tour2 depth2 first2]
                               (let [[t dd f] (cart-euler left right (nth left u) (+ d 1) tour2 depth2 first2)]
                                 [(conj t u) (conj dd d) f]))
        [tour4 depth4 first4] (if (= (nth right u) -1) [tour3 depth3 first3]
                               (let [[t dd f] (cart-euler left right (nth right u) (+ d 1) tour3 depth3 first3)]
                                 [(conj t u) (conj dd d) f]))]
    [tour4 depth4 first4]))

(defn build-st [depth m]
  (let [lg (loop [i 2 l (assoc (vec (repeat (+ m 1) 0)) 0 0 1 0)]
             (if (> i m) l (recur (+ i 1) (assoc l i (+ (nth l (quot i 2)) 1)))))
        st0 (vec (range m))]
    [(loop [k 1 st [st0]]
       (if (> k (nth lg m)) st
         (let [prev (nth st (- k 1))
               row (mapv (fn [i] (let [x (nth prev i) y (nth prev (+ i (bit-shift-left 1 (- k 1))))]
                                   (if (<= (nth depth x) (nth depth y)) x y)))
                         (range (+ (- m (bit-shift-left 1 k)) 1)))]
           (recur (+ k 1) (conj st row))))) lg]))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        a (mapv (fn [i] (read-int (nth toks (+ 1 i)))) (range n))
        [left right parent] (build-cartesian a n)
        root (loop [i 0] (if (= (nth parent i) -1) i (recur (+ i 1))))
        [tour depth first] (cart-euler left right root 0 [] [] (vec (repeat n -1)))
        m (count tour)
        [st lg] (build-st depth m)
        q (read-int (nth toks (+ 1 n)))]
    (mapv (fn [qi] (let [l (read-int (nth toks (+ 2 n (* qi 2))))
                         r (read-int (nth toks (+ 3 n (* qi 2))))
                         i0 (nth first l) j0 (nth first r)
                         i (min i0 j0) j (max i0 j0)
                         k (nth lg (+ (- j i) 1))
                         x (nth (nth st k) i) y (nth (nth st k) (+ (- j (bit-shift-left 1 k)) 1))]
                     (str (nth a (nth tour (if (<= (nth depth x) (nth depth y)) x y))))))
          (range q))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
