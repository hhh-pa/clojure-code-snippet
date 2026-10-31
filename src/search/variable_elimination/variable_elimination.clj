
;; variable_elimination: Bayesian network exact inference by variable
;; elimination. Binary variables, CPT rows as exact fractions; factors
;; support restriction, point-wise product and summing out.
(ns variable-elimination
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn gcd* [a b]
  (let [a (if (neg? a) (- a) a) b (if (neg? b) (- b) b)]
    (loop [x a y b]
      (if (zero? y) (if (zero? x) 1 x) (recur y (rem x y))))))

(defn norm* [p q]
  (let [[p q] (if (neg? q) [(- p) (- q)] [p q])
        g (gcd* p q)]
    [(quot p g) (quot q g)]))

(defn fmul [[p1 q1] [p2 q2]] (norm* (* p1 p2) (* q1 q2)))
(defn fadd [[p1 q1] [p2 q2]] (norm* (+ (* p1 q2) (* p2 q1)) (* q1 q2)))

(defn bit-at [i from-left sz]
  (bit-and (bit-shift-right i (- (dec sz) from-left)) 1))

(defn restrict-ev [fac evid]
  (reduce (fn [fs e]
            (if (neg? (nth evid e))
              fs
              (mapv (fn [f]
                      (let [pos (.indexOf ^java.util.List (:scope f) e)]
                        (if (neg? pos)
                          f
                          (let [sz (count (:scope f))
                                ns (vec (concat (subvec (:scope f) 0 pos) (subvec (:scope f) (inc pos))))
                                nt (vec (for [i (range (bit-shift-left 1 sz))
                                              :when (= (bit-at i pos sz) (nth evid e))]
                                          (nth (:tab f) i)))]
                            {:scope ns :tab nt}))))
                    fs)))
          fac (range (count evid))))

(defn prod-factors [fs]
  (let [sc (vec (sort (distinct (mapcat :scope fs))))
        sz (count sc)
        idx-of (zipmap sc (range))]
    {:scope sc
     :tab (vec (for [i (range (bit-shift-left 1 sz))]
                 (reduce (fn [pr f]
                           (let [fs2 (:scope f)
                                 idx (reduce (fn [a j]
                                               (bit-or (bit-shift-left a 1)
                                                       (bit-at i (idx-of (nth fs2 j)) sz)))
                                             0 (range (count fs2)))]
                             (fmul pr (nth (:tab f) idx))))
                         [1 1] fs)))}))

(defn sum-out [f v]
  (let [sc (:scope f)
        pos (.indexOf ^java.util.List sc v)
        sz (count sc)
        ns (vec (concat (subvec sc 0 pos) (subvec sc (inc pos))))]
    {:scope ns
     :tab (let [acc (vec (repeat (bit-shift-left 1 (dec sz)) [0 1]))]
            (reduce (fn [a i]
                      (let [ni (reduce (fn [x j]
                                          (if (= j pos) x (bit-or (bit-shift-left x 1) (bit-at i j sz))))
                                        0 (range sz))]
                        (assoc a ni (fadd (nth a ni) (nth (:tab f) i)))))
                    acc (range (bit-shift-left 1 sz))))}))

(defn solve-one [n parents cpt q evid]
  (let [facs (vec (for [v (range n)]
                    (let [sc (vec (sort (conj (nth parents v) v)))
                          sz (count sc)
                          idx-of (zipmap sc (range))
                          k (count (nth parents v))]
                      {:scope sc
                       :tab (vec (for [i (range (bit-shift-left 1 sz))]
                                   (let [vv (bit-at i (idx-of v) sz)
                                         row (reduce (fn [a j]
                                                       (bit-or a (bit-shift-left
                                                                   (bit-at i (idx-of (nth (nth parents v) j)) sz)
                                                                   (- (dec k) j))))
                                                     0 (range k))
                                         pr (nth (nth cpt v) row)]
                                     (if (= vv 1) pr (norm* (- (second pr) (first pr)) (second pr))))))})))
        facs2 (restrict-ev facs evid)
        hidden (remove #(or (= % q) (not (neg? (nth evid %)))) (range n))
        facs3 (reduce (fn [fs v]
                        (let [has (filterv #(some (fn [x] (= x v)) (:scope %)) fs)
                              rest (filterv #(not (some (fn [x] (= x v)) (:scope %))) fs)]
                          (if (empty? has)
                            rest
                            (conj rest (sum-out (prod-factors has) v)))))
                      facs2 hidden)
        joint (prod-factors facs3)
        sc (:scope joint)
        pos (.indexOf ^java.util.List sc q)
        sz (count sc)
        [jt jf] (reduce (fn [[t f] i]
                          (let [b (bit-at i pos sz)]
                            (if (= b 1)
                              [(fadd t (nth (:tab joint) i)) f]
                              [t (fadd f (nth (:tab joint) i))])))
                        [[0 1] [0 1]] (range (bit-shift-left 1 (max sz 1))))]
    (let [tot (fadd jt jf)]
      (if (zero? (first tot))
        [0 1]
        (norm* (* (first jt) (second tot)) (* (second jt) (first tot)))))))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos))]
    (loop [v 0 p (inc pos) parents [] cpt []]
      (if (= v n)
        (let [q (int (nth nums p)) ne (int (nth nums (inc p)))
              p2 (+ p 2)
              evid (loop [i 0 pp p2 ev (vec (repeat n -1))]
                     (if (= i ne)
                       [ev pp]
                       (recur (inc i) (+ pp 2)
                              (assoc ev (int (nth nums pp)) (int (nth nums (inc pp)))))))]
          [n parents cpt q (first evid) (second evid)])
        (let [k (int (nth nums p))
              pl (vec (map int (subvec nums (inc p) (+ p 1 k))))
              p1 (+ p 1 k)
              rows (bit-shift-left 1 k)
              cp (vec (for [r (range rows)]
                        (norm* (nth nums (+ p1 (* r 2))) (nth nums (+ p1 (* r 2) 1)))))]
          (recur (inc v) (+ p1 (* rows 2)) (conj parents pl) (conj cpt cp)))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n parents cpt q evid p2] (parse-case nums pos)
              [a b] (solve-one n parents cpt q evid)]
          (.append sb (str "VE " a "/" b "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
