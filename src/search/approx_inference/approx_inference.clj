
;; approx_inference: Bayesian network approximate inference -- rejection
;; sampling, likelihood weighting and Gibbs MCMC -- over binary networks
;; with exact-fraction CPTs. Seeded LCG, fresh stream per method.
(ns approx-inference
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)
(def ^:private two53-inv 1.1102230246251565E-16)

(defn lcg-next [s] (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))
(defn lcg-int [s n]
  (let [ns (lcg-next s)] [ns (int (Long/remainderUnsigned ns n))]))
(defn lcg-double [s]
  (let [ns (lcg-next s)]
    [ns (* (double (unsigned-bit-shift-right ns 11)) two53-inv)]))

(defn prow [v asn parents cpt]
  (let [pl (nth parents v)
        k (count pl)
        row (reduce (fn [a j] (bit-or a (bit-shift-left (nth asn (nth pl j)) (- (dec k) j))))
                    0 (range k))]
    (nth (nth cpt v) row)))

(defn gibbs-prob [v asn parents children cpt]
  (let [a1 (assoc asn v 1)
        p1 (prow v a1 parents cpt)
        pt (reduce (fn [a c] (let [p (prow c a1 parents cpt)]
                               (* a (if (= 1 (nth a1 c)) p (- 1.0 p)))))
                   p1 (nth children v))
        a0 (assoc asn v 0)
        p0 (- 1.0 (prow v a0 parents cpt))
        pv (reduce (fn [a c] (let [p (prow c a0 parents cpt)]
                               (* a (if (= 1 (nth a0 c)) p (- 1.0 p)))))
                   p0 (nth children v))]
    (if (zero? (+ pt pv))
      (if (= 1 (nth asn v)) 1.0 0.0)
      (/ pt (+ pt pv)))))

(defn reject-samp [n parents cpt q evid s seed]
  (loop [i 0 rng seed acc 0 qt 0]
    (if (= i s)
      [acc qt]
      (let [[asn rng2] (loop [v 0 a (vec (repeat n 0)) r rng]
                         (if (= v n)
                           [a r]
                           (let [[r2 u] (lcg-double r)]
                             (recur (inc v) (assoc a v (if (< u (prow v a parents cpt)) 1 0)) r2))))
            ok (every? (fn [v] (or (neg? (nth evid v)) (= (nth asn v) (nth evid v)))) (range n))]
        (recur (inc i) rng2 (if ok (inc acc) acc) (if (and ok (= 1 (nth asn q))) (inc qt) qt))))))

(defn likelihood-w [n parents cpt q evid s seed]
  (loop [i 0 rng seed wt 0.0 wq 0.0]
    (if (= i s)
      (if (zero? wt) 0.0 (/ wq wt))
      (let [[asn rng2 w] (loop [v 0 a (vec (repeat n 0)) r rng w 1.0]
                           (if (= v n)
                             [a r w]
                             (if (not (neg? (nth evid v)))
                               (let [a2 (assoc a v (nth evid v))
                                     p (prow v a2 parents cpt)]
                                 (recur (inc v) a2 r (* w (if (= 1 (nth evid v)) p (- 1.0 p)))))
                               (let [[r2 u] (lcg-double r)]
                                 (recur (inc v) (assoc a v (if (< u (prow v a parents cpt)) 1 0)) r2 w)))))]
        (recur (inc i) rng2 (+ wt w) (if (= 1 (nth asn q)) (+ wq w) wq))))))

(defn gibbs-sweep [asn rng parents children cpt evid]
  (loop [v 0 a asn r rng]
    (if (= v (count a))
      [a r]
      (if (not (neg? (nth evid v)))
        (recur (inc v) a r)
        (let [pr (gibbs-prob v a parents children cpt)
              [r2 u] (lcg-double r)]
          (recur (inc v) (assoc a v (if (< u pr) 1 0)) r2))))))

(defn gibbs [n parents children cpt q evid s seed]
  (let [[asn0 rng0] (loop [v 0 a (vec (repeat n 0)) r seed]
                      (if (= v n)
                        [a r]
                        (if (not (neg? (nth evid v)))
                          (recur (inc v) (assoc a v (nth evid v)) r)
                          (let [[r2 b] (lcg-int r 2)]
                            (recur (inc v) (assoc a v b) r2)))))
        [asnb rngb] (loop [b 0 a asn0 r rng0]
                      (if (= b 10)
                        [a r]
                        (let [[a2 r2] (gibbs-sweep a r parents children cpt evid)]
                          (recur (inc b) a2 r2))))]
    (loop [i 0 a asnb r rngb qt 0]
      (if (= i s)
        qt
        (let [[a2 r2] (gibbs-sweep a r parents children cpt evid)]
          (recur (inc i) a2 r2 (if (= 1 (nth a2 q)) (inc qt) qt)))))))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos))]
    (loop [v 0 p (inc pos) parents [] cpt [] children (vec (repeat n []))]
      (if (= v n)
        (let [q (int (nth nums p)) ne (int (nth nums (inc p)))
              p2 (+ p 2)
              [ev p3] (loop [i 0 pp p2 ev (vec (repeat n -1))]
                        (if (= i ne)
                          [ev pp]
                          (recur (inc i) (+ pp 2)
                                 (assoc ev (int (nth nums pp)) (int (nth nums (inc pp)))))))
              s (int (nth nums p3)) seed (nth nums (inc p3))]
          [n parents cpt children q ev s seed (+ p3 2)])
        (let [k (int (nth nums p))
              pl (vec (map int (subvec nums (inc p) (+ p 1 k))))
              p1 (+ p 1 k)
              rows (bit-shift-left 1 k)
              cp (vec (for [r (range rows)]
                        (/ (double (nth nums (+ p1 (* r 2)))) (double (nth nums (+ p1 (* r 2) 1))))))
              ch2 (reduce (fn [ch x] (update ch x conj v)) children pl)]
          (recur (inc v) (+ p1 (* rows 2)) (conj parents pl) (conj cpt cp) ch2))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n parents cpt children q evid s seed p2] (parse-case nums pos)
              [acc qt] (reject-samp n parents cpt q evid s seed)
              lw (likelihood-w n parents cpt q evid s seed)
              gq (gibbs n parents children cpt q evid s seed)]
          (.append sb (str "REJECT " acc " " qt "\n"))
          (.append sb (str "LIKELIHOOD " (String/format java.util.Locale/US "%.6f"
                                                        (into-array Object [(double lw)])) "\n"))
          (.append sb (str "GIBBS " gq " " s "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
