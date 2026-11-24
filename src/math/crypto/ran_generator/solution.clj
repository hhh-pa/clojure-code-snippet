;; Uniform random generators (NR 7.1).
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

(def VINIT 4101842887655102017)

(defn u+ [a b] (unchecked-add a b))
(defn u* [a b] (unchecked-multiply a b))

(defn u64-str [v] (Long/toUnsignedString v))

(defn u64-dbl [v] (Double/parseDouble (Long/toUnsignedString v)))

(defn ran-step [u v w]
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
  (let [[u1 _v1 w1 _] (ran-step (bit-xor j VINIT) VINIT 1)
        [u2 v2 _w2 _] (ran-step u1 u1 w1)
        [u3 v3 w3 _] (ran-step u2 v2 v2)]
    [u3 v3 w3]))

(defn ranq1-step [v]
  (let [v (bit-xor v (unsigned-bit-shift-right v 21))
        v (bit-xor v (bit-shift-left v 35))
        v (bit-xor v (unsigned-bit-shift-right v 4))]
    [v (u* v 2685821657736338717)]))

(defn ranq1-init [j]
  (let [v (bit-xor VINIT j)]
    (second (ranq1-step v))))

(defn ranq2-step [v w]
  (let [v (bit-xor v (unsigned-bit-shift-right v 17))
        v (bit-xor v (bit-shift-left v 31))
        v (bit-xor v (unsigned-bit-shift-right v 8))
        w (u+ (u* 4294957665 (bit-and w 0xFFFFFFFF)) (unsigned-bit-shift-right w 32))]
    [v w (bit-xor v w)]))

(defn ranq2-init [j]
  (let [v (bit-xor VINIT j)
        [v _w o1] (ranq2-step v 1)
        w o1
        [_v w o2] (ranq2-step v w)
        v o2]
    [v w]))

(defn ranhash64 [u]
  (let [v (u+ (u* u 3935559000370003845) 2691343689449507681)
        v (bit-xor v (unsigned-bit-shift-right v 21))
        v (bit-xor v (bit-shift-left v 37))
        v (bit-xor v (unsigned-bit-shift-right v 4))
        v (u* v 4768777513237032717)
        v (bit-xor v (bit-shift-left v 20))
        v (bit-xor v (unsigned-bit-shift-right v 41))
        v (bit-xor v (bit-shift-left v 5))]
    v))

(defn ranfib-init [j]
  (loop [k 0 v (ranq1-init j) tab []]
    (if (>= k 55)
      [v tab]
      (let [[nv o] (ranq1-step v)]
        (recur (inc k) nv (conj tab (* 5.42101086242752217E-20 (u64-dbl o))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          pu (fn [i] (Long/parseUnsignedLong (nth toks i)))
          pi (fn [i] (Long/parseLong (nth toks i)))]
      (println
       (str/join " "
        (cond
          (= op "RAN") (let [n (pi 2) [u v w] (ran-init (pu 1))]
                         (loop [i 0 u u v v w w acc []]
                           (if (>= i n)
                             acc
                             (let [[u v w o] (ran-step u v w)]
                               (recur (inc i) u v w (conj acc (u64-str o)))))))
          (= op "RAN_D") (let [n (pi 2) [u v w] (ran-init (pu 1))]
                           (loop [i 0 u u v v w w acc []]
                             (if (>= i n)
                               acc
                               (let [[u v w o] (ran-step u v w)]
                                 (recur (inc i) u v w
                                        (conj acc (fmt6 (canon (* 5.42101086242752217E-20 (u64-dbl o))))))))))
          (= op "RAN32") (let [n (pi 2) [u v w] (ran-init (pu 1))]
                           (loop [i 0 u u v v w w acc []]
                             (if (>= i n)
                               acc
                               (let [[u v w o] (ran-step u v w)]
                                 (recur (inc i) u v w (conj acc (u64-str (bit-and o 0xFFFFFFFF))))))))
          (= op "RAN8") (let [n (pi 2) [u v w] (ran-init (pu 1))]
                          (loop [i 0 u u v v w w breg 0 bc 0 acc []]
                            (if (>= i n)
                              acc
                              (let [old bc
                                    bc (dec bc)]
                                (if (not= old 0)
                                  (let [breg (unsigned-bit-shift-right breg 8)]
                                    (recur (inc i) u v w breg bc (conj acc (str (bit-and breg 0xFF)))))
                                  (let [[u v w o] (ran-step u v w)]
                                    (recur (inc i) u v w o 7 (conj acc (str (bit-and o 0xFF))))))))))
          (= op "RANQ1") (let [n (pi 2)]
                           (loop [i 0 v (ranq1-init (pu 1)) acc []]
                             (if (>= i n)
                               acc
                               (let [[nv o] (ranq1-step v)]
                                 (recur (inc i) nv (conj acc (u64-str o)))))))
          (= op "RANQ1_D") (let [n (pi 2)]
                             (loop [i 0 v (ranq1-init (pu 1)) acc []]
                               (if (>= i n)
                                 acc
                                 (let [[nv o] (ranq1-step v)]
                                   (recur (inc i) nv
                                          (conj acc (fmt6 (canon (* 5.42101086242752217E-20 (u64-dbl o))))))))))
          (= op "RANQ2") (let [n (pi 2) [v w] (ranq2-init (pu 1))]
                           (loop [i 0 v v w w acc []]
                             (if (>= i n)
                               acc
                               (let [[v w o] (ranq2-step v w)]
                                 (recur (inc i) v w (conj acc (u64-str o)))))))
          (= op "RANQ2_D") (let [n (pi 2) [v w] (ranq2-init (pu 1))]
                             (loop [i 0 v v w w acc []]
                               (if (>= i n)
                                 acc
                                 (let [[v w o] (ranq2-step v w)]
                                   (recur (inc i) v w
                                          (conj acc (fmt6 (canon (* 5.42101086242752217E-20 (u64-dbl o))))))))))
          (= op "HASH64") [(u64-str (ranhash64 (pu 1)))]
          (= op "HASH32") [(u64-str (bit-and (ranhash64 (pu 1)) 0xFFFFFFFF))]
          (= op "HASH_D") [(fmt6 (canon (* 5.42101086242752217E-20 (u64-dbl (ranhash64 (pu 1))))))]
          (= op "RANFIB_D") (let [n (pi 2) [_ tab] (ranfib-init (pu 1))]
                              (loop [i 0 ix 0 ixp 31 tab (vec tab) acc []]
                                (if (>= i n)
                                  acc
                                  (let [ix (if (= (inc ix) 55) 0 (inc ix))
                                        ixp (if (= (inc ixp) 55) 0 (inc ixp))
                                        dd (- (nth tab ix) (nth tab ixp))
                                        dd (if (neg? dd) (+ dd 1.0) dd)]
                                    (recur (inc i) ix ixp (assoc tab ix dd)
                                           (conj acc (fmt6 (canon dd))))))))
          (= op "RANFIB_32") (let [n (pi 2) [_ tab] (ranfib-init (pu 1))]
                               (loop [i 0 ix 0 ixp 31 tab (vec tab) acc []]
                                 (if (>= i n)
                                   acc
                                   (let [ix (if (= (inc ix) 55) 0 (inc ix))
                                         ixp (if (= (inc ixp) 55) 0 (inc ixp))
                                         dd (- (nth tab ix) (nth tab ixp))
                                         dd (if (neg? dd) (+ dd 1.0) dd)]
                                     (recur (inc i) ix ixp (assoc tab ix dd)
                                            (conj acc (str (long (* dd 4294967295.0)))))))))))))))
