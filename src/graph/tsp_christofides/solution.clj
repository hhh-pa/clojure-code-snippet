;; Christofides 3/2 TSP: MST + subset-DP matching + Euler + shortcut.
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- cf-find
  [parent a]
  (loop [x a]
    (let [p (nth parent x)]
      (if (= p x)
        x
        (recur p)))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          kind (nth toks 1)
          w (if (= kind "LINE")
              (let [xs (mapv #(Double/parseDouble %) (subvec toks 2 (+ 2 n)))]
                (mapv (fn [i] (mapv (fn [j] (Math/abs (- (nth xs i) (nth xs j))))
                                    (range n)))
                      (range n)))
              (let [px (mapv #(Double/parseDouble %) (map #(nth toks %) (range 2 (+ 2 (* 2 n)) 2)))
                    py (mapv #(Double/parseDouble %) (map #(nth toks %) (range 3 (+ 3 (* 2 n)) 2)))]
                (mapv (fn [i]
                        (mapv (fn [j]
                                (let [dx (- (nth px i) (nth px j))
                                      dy (- (nth py i) (nth py j))]
                                  (Math/sqrt (+ (* dx dx) (* dy dy)))))
                              (range n)))
                      (range n))))
          edges (sort (for [i (range n) j (range (inc i) n)] [(nth (nth w i) j) i j]))
          parent (vec (range n))
          [mst _] (reduce (fn [[acc p] [_ u v]]
                            (let [ru (cf-find p u) rv (cf-find p v)]
                              (if (= ru rv)
                                [acc p]
                                [(conj acc [u v]) (assoc p ru rv)])))
                          [[] parent] edges)
          deg (reduce (fn [d [u v]] (-> d (update u inc) (update v inc)))
                      (vec (repeat n 0)) mst)
          odd (vec (filter #(odd? (nth deg %)) (range n)))
          k (count odd)
          choice (loop [mask 1 d (assoc (vec (repeat (bit-shift-left 1 k) Double/POSITIVE_INFINITY)) 0 0.0)
                       ch (vec (repeat (bit-shift-left 1 k) 0))]
                 (if (>= mask (bit-shift-left 1 k))
                   ch
                 (let [i (loop [b 0] (if (pos? (bit-and mask (bit-shift-left 1 b))) b (recur (inc b))))
                       rest (bit-xor mask (bit-shift-left 1 i))
                       [best bj] (loop [j (inc i) b Double/POSITIVE_INFINITY bj 0]
                                   (if (>= j k)
                                     [b bj]
                                     (if (pos? (bit-and rest (bit-shift-left 1 j)))
                                       (let [c (+ (nth (nth w (nth odd i)) (nth odd j))
                                                  (nth d (bit-xor rest (bit-shift-left 1 j))))]
                                         (if (< c b)
                                           (recur (inc j) c j)
                                           (recur (inc j) b bj)))
                                       (recur (inc j) b bj))))]
                   (recur (inc mask) (assoc d mask best) (assoc ch mask bj)))))
          [adj ne] (loop [a (vec (repeat n [])) es mst e 0]
                     (if (empty? es)
                       [a e]
                       (let [[u v] (first es)]
                         (recur (-> a (update u conj [v e]) (update v conj [u e]))
                                (rest es) (inc e)))))
          adj (loop [mask (dec (bit-shift-left 1 k)) a adj e ne]
                (if (zero? mask)
                  a
                  (let [i (loop [b 0] (if (pos? (bit-and mask (bit-shift-left 1 b))) b (recur (inc b))))
                        j (nth choice mask)]
                    (recur (bit-xor mask (bit-shift-left 1 i) (bit-shift-left 1 j))
                           (-> a
                               (update (nth odd i) conj [(nth odd j) e])
                               (update (nth odd j) conj [(nth odd i) e]))
                           (inc e)))))
          adj (mapv (fn [lst] (vec (reverse (sort lst)))) adj)
          tour (loop [stack [0] t [] a adj used #{}]
                 (if (empty? stack)
                   t
                   (let [v (peek stack)
                         clean (loop [lst (nth a v)]
                                 (if (and (seq lst) (contains? used (peek (peek lst))))
                                   (recur (pop lst))
                                   lst))
                         a (assoc a v clean)]
                     (if (empty? (nth a v))
                       (recur (pop stack) (conj t v) a used)
                       (let [[u e] (peek (nth a v))]
                         (recur (conj stack u) t (update a v pop) (conj used e)))))))
          tour (vec (reverse tour))
          sht (loop [vs tour seen #{} acc []]
                (if (empty? vs)
                  acc
                  (let [v (first vs)]
                    (if (seen v)
                      (recur (rest vs) seen acc)
                      (recur (rest vs) (conj seen v) (conj acc v))))))
          length (reduce + (map (fn [i] (nth (nth w (nth sht i)) (nth sht (mod (inc i) n))))
                                (range n)))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str (str/join " " sht) "\n" (fmt length))))))
