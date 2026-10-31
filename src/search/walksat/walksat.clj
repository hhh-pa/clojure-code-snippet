
;; walksat: WalkSAT stochastic local search for SAT. Random init from a
;; seeded LCG; each flip targets a random unsatisfied clause, flipping a
;; random variable with probability p or the min-break variable.
(ns walksat
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)

(defn lcg-next [s] (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))
(defn lcg-int [s n]
  (let [ns (lcg-next s)] [ns (int (Long/remainderUnsigned ns n))]))

(defn sat-lit? [l asn]
  (let [x (dec (Math/abs (int l)))]
    (= (= (nth asn x) 1) (pos? l))))

(defn sat-cls? [c asn] (boolean (some #(sat-lit? % asn) c)))
(defn all-sat? [clauses asn] (every? #(sat-cls? % asn) clauses))

(defn break-count [clauses asn x]
  (let [a2 (update asn x bit-xor 1)]
    (count (for [c clauses :when (and (not (sat-cls? c a2)) (sat-cls? c asn))] 1))))

(defn walksat [v clauses mf pn pd seed]
  (let [[rng0 asn0] (loop [r seed i 0 a []]
                      (if (= i v) [r a]
                          (let [[r2 b] (lcg-int r 2)]
                            (recur r2 (inc i) (conj a b)))))]
    (loop [asn asn0 rng rng0 f 0]
      (if (all-sat? clauses asn)
        [true asn]
        (if (>= f mf)
          [false asn]
          (let [unsat (vec (for [i (range (count clauses)) :when (not (sat-cls? (nth clauses i) asn))] i))
                [rng1 ui] (lcg-int rng (count unsat))
                cl (nth clauses (nth unsat ui))
                rng2 (lcg-next rng1)]
            (if (< (Long/remainderUnsigned rng2 pd) pn)
              (let [[rng3 vi] (lcg-int rng2 (count cl))
                    pick (dec (Math/abs (int (nth cl vi))))]
                (recur (update asn pick bit-xor 1) rng3 (inc f)))
              (let [[pick _] (reduce (fn [[bp bb] l]
                                       (let [x (dec (Math/abs (int l)))
                                             br (break-count clauses asn x)]
                                         (if (< br bb) [x br] [bp bb])))
                                     [0 1000000000] cl)]
                (recur (update asn pick bit-xor 1) rng2 (inc f))))))))))

(defn parse-case [nums pos]
  (let [v (int (nth nums pos)) c (int (nth nums (+ pos 1)))
        mf (int (nth nums (+ pos 2))) pn (nth nums (+ pos 3)) pd (nth nums (+ pos 4))
        seed (nth nums (+ pos 5))]
    (loop [i 0 p (+ pos 6) cls []]
      (if (= i c)
        [v cls mf pn pd seed p]
        (let [k (int (nth nums p))]
          (recur (inc i) (+ p 1 k) (conj cls (vec (map int (subvec nums (inc p) (+ p 1 k)))))))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[v cls mf pn pd seed p] (parse-case nums pos)
              [ok asn] (walksat v cls mf pn pd seed)]
          (.append sb (str "WALKSAT " (if ok "SAT" "UNKNOWN") " " (str/join " " asn) "\n"))
          (recur p (dec tcases) sb))))))

(-main)
