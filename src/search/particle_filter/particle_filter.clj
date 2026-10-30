
;; particle_filter: discrete-state particle filter (sequential Monte
;; Carlo). Particles propagate through the transition model, are
;; weighted by the sensor model and multinomially resampled each step.
(ns particle-filter
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

(defn sample-next [row u]
  (loop [j 0 c 0.0]
    (if (>= j (count row))
      (dec (count row))
      (let [c2 (+ c (nth row j))]
        (if (< u c2) j (recur (inc j) c2))))))

(defn sample-cum [cw u]
  (let [n (dec (count cw))]
    (loop [j 0]
      (if (or (>= j n) (< u (nth cw (inc j))))
        (if (>= j n) (dec n) j)
        (recur (inc j))))))

(defn solve-one [ns no n tl seed tr se obs]
  (let [[parts rng] (loop [i 0 a [] r seed]
                      (if (= i n)
                        [a r]
                        (let [[r2 b] (lcg-int r ns)]
                          (recur (inc i) (conj a b) r2))))]
    (loop [k 0 ps parts r rng hists []]
      (if (= k tl)
        hists
        (let [[ps2 r2] (loop [i 0 a ps rr r]
                         (if (= i n)
                           [a rr]
                           (let [[rr2 u] (lcg-double rr)]
                             (recur (inc i) (assoc a i (sample-next (nth tr (nth a i)) u)) rr2))))
              w (mapv #(nth (nth se %) (nth obs k)) ps2)
              tot (reduce + 0.0 w)
              [ps3 r3] (if (zero? tot)
                         (loop [i 0 a ps2 rr r2]
                           (if (= i n)
                             [a rr]
                             (let [[rr2 b] (lcg-int rr ns)]
                               (recur (inc i) (assoc a i b) rr2))))
                         (let [cw (vec (reductions + 0.0 (map #(/ % tot) w)))]
                           (loop [i 0 a ps2 rr r2]
                             (if (= i n)
                               [a rr]
                               (let [[rr2 u] (lcg-double rr)]
                                 (recur (inc i) (assoc a i (nth ps2 (sample-cum cw u))) rr2))))))
              hist (reduce (fn [h x] (update h x inc)) (vec (repeat ns 0)) ps3)]
          (recur (inc k) ps3 r3 (conj hists hist)))))))

(defn parse-case [nums pos]
  (let [ns (int (nth nums pos)) no (int (nth nums (+ pos 1)))
        n (int (nth nums (+ pos 2))) tl (int (nth nums (+ pos 3)))
        seed (nth nums (+ pos 4))
        p0 (+ pos 5)
        fr (fn [p] (/ (double (nth nums p)) (double (nth nums (inc p)))))
        tr (vec (for [i (range ns)] (vec (for [j (range ns)] (fr (+ p0 (* (+ (* i ns) j) 2)))))))
        p1 (+ p0 (* ns ns 2))
        se (vec (for [i (range ns)] (vec (for [j (range no)] (fr (+ p1 (* (+ (* i no) j) 2)))))))
        p2 (+ p1 (* ns no 2))
        obs (vec (map int (subvec nums p2 (+ p2 tl))))]
    [ns no n tl seed tr se obs (+ p2 tl)]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[ns no n tl seed tr se obs p2] (parse-case nums pos)
              hists (solve-one ns no n tl seed tr se obs)]
          (.append sb "PF\n")
          (doseq [h hists]
            (.append sb (str (str/join " " h) "\n")))
          (recur p2 (dec tcases) sb))))))

(-main)
