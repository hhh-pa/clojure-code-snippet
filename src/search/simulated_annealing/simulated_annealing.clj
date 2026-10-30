;; simulated_annealing: N-queens stochastic local search with Boltzmann
;; acceptance exp(-delta/T) and geometric cooling. Mirrors the C++ port:
;; splitmix-style LCG, unsigned nextInt, exp acceptance, best tracking.
(ns simulated-annealing
  (:require [clojure.string :as str]))

(defn attacking [board]
  (let [n (count board)]
    (count (for [i (range n) j (range (inc i) n)
                 :when (let [bi (nth board i) bj (nth board j)]
                         (or (= bi bj)
                             (= (Math/abs (- bi bj)) (Math/abs (- i j)))))]
             1))))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)
(def ^:private two53-inv 1.1102230246251565E-16)

(defn lcg-next [s]
  (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))

(defn lcg-int [s n]
  (let [ns (lcg-next s)]
    [ns (int (Long/remainderUnsigned ns n))]))

(defn lcg-double [s]
  (let [ns (lcg-next s)]
    [ns (* (double (unsigned-bit-shift-right ns 11)) two53-inv)]))

(defn anneal [board seed]
  (let [n (count board)]
    (loop [b board cur (attacking board) best board bestc (attacking board)
           temp 100.0 rng seed steps 0]
      (if (or (<= temp 0.01) (>= steps 5000))
        [best bestc]
        (let [[rng1 r] (lcg-int rng n)
              [rng2 c0] (lcg-int rng1 n)
              c (if (= c0 (nth b r)) (mod (inc c0) n) c0)
              old (nth b r)
              nb (assoc b r c)
              nc (attacking nb)
              delta (- nc cur)]
          (if (< delta 0)
            (let [nbest (if (< nc bestc) nb best)
                  nbestc (min nc bestc)]
              (recur nb nc nbest nbestc (* temp 0.99) rng2 (inc steps)))
            (let [[rng3 d] (lcg-double rng2)]
              (if (< d (Math/exp (/ (- (double delta)) temp)))
                (let [nbest (if (< nc bestc) nb best)
                      nbestc (min nc bestc)]
                  (recur nb nc nbest nbestc (* temp 0.99) rng3 (inc steps)))
                (recur b cur best bestc (* temp 0.99) rng3 (inc steps))))))))))

(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        nums (mapv #(Long/parseLong %) toks)
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (nth nums pos)
              board (vec (map int (subvec nums (inc pos) (+ pos 1 n))))
              seed (nth nums (+ pos 1 n))
              [best bestc] (anneal board seed)]
          (.append sb (str/join " " best))
          (.append sb "\n")
          (.append sb (str bestc))
          (.append sb "\n")
          (recur (+ pos 2 n) (dec tcases) sb))))))

(-main)
