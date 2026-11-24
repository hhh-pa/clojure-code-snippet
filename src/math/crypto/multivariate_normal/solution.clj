;; Multivariate normal deviates (NR 7.4).
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

(defn u+ [a b] (unchecked-add a b))
(defn u* [a b] (unchecked-multiply a b))

(def VINIT 4101842887655102017)
(def DOUBC 5.42101086242752217E-20)

(defn u64-dbl [v] (Double/parseDouble (Long/toUnsignedString v)))

(defn ran-step [[u v w]]
  (let [u (u+ (u* u 2862933555777941757) 7046029254386353087)
        v (bit-xor v (unsigned-bit-shift-right v 17))
        v (bit-xor v (bit-shift-left v 31))
        v (bit-xor v (unsigned-bit-shift-right v 8))
        w (u+ (u* 4294957665 (bit-and w 0xFFFFFFFF)) (unsigned-bit-shift-right w 32))
        x (bit-xor u (bit-shift-left u 21))
        x (bit-xor x (unsigned-bit-shift-right x 35))
        x (bit-xor x (bit-shift-left x 4))]
    [u v w (bit-xor (u+ x v) w)]))

(defn ran-init [j]
  (let [[u1 _ w1 _] (ran-step [(bit-xor j VINIT) VINIT 1])
        [u2 v2 _ _] (ran-step [u1 u1 w1])
        [u3 v3 w3 _] (ran-step [u2 v2 v2])]
    [u3 v3 w3]))

(defn ran-doub [st]
  (let [[u v w o] (ran-step st)]
    [[u v w] (* DOUBC (u64-dbl o))]))

(defn cholesky [a mm]
  (let [l (vec (repeat mm (vec (repeat mm 0.0))))]
    (loop [i 0 l l]
      (if (>= i mm)
        l
        (recur (inc i)
               (loop [j 0 l l]
                 (if (> j i)
                   l
                   (let [s (loop [k 0 s (nth (nth a i) j)]
                             (if (>= k j)
                               s
                               (recur (inc k) (- s (* (nth (nth l i) k) (nth (nth l j) k))))))]
                     (recur (inc j) (assoc-in l [i j] (if (= i j) (Math/sqrt s) (/ s (nth (nth l j) j)))))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          mm (Long/parseLong (next-tok))
          mean (vec (repeatedly mm #(Double/parseDouble (next-tok))))
          a (vec (for [_ (range mm)] (vec (for [_ (range mm)] (Double/parseDouble (next-tok))))))
          seed (Long/parseUnsignedLong (next-tok))
          n (Long/parseLong (next-tok))
          l (cholesky a mm)]
      (loop [d 0 st (ran-init seed) lines []]
        (if (>= d n)
          (println (str/join "\n" lines))
          (let [[st spt] (loop [i 0 st st spt []]
                           (if (>= i mm)
                             [st spt]
                             (let [[st sn] (loop [st st]
                                             (let [[st u] (ran-doub st)
                                                   [st vd] (ran-doub st)
                                                   v (* 1.7156 (- vd 0.5))
                                                   x (- u 0.449871)
                                                   y (+ (absv v) 0.386595)
                                                   q (+ (* x x) (* y (- (* 0.19600 y) (* 0.25472 x))))]
                                               (if (not (and (> q 0.27597)
                                                             (or (> q 0.27846)
                                                                 (> (* v v) (* (* -4.0 (Math/log u)) (* u u))))))
                                                 [st (/ v u)]
                                                 (recur st))))]
                               (recur (inc i) st (conj spt sn)))))
                pt (vec (for [i (range mm)]
                          (+ (loop [j 0 s 0.0]
                               (if (> j i)
                                 s
                                 (recur (inc j) (+ s (* (nth (nth l i) j) (nth spt j))))))
                             (nth mean i))))]
            (recur (inc d) st (conj lines (str/join " " (map #(fmt6 (canon %)) pt))))))))))
