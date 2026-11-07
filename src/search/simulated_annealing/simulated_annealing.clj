;; simulated_annealing: N-queens stochastic local search with Boltzmann
;; acceptance exp(-delta/T) and geometric cooling. Mirrors the C++ port:
;; splitmix-style LCG, unsigned nextInt, exp acceptance, best tracking.
(ns simulated-annealing
  (:require [clojure.string :as str]))

(defn attacking [board]
  (let [n (count board)]
    (loop [i 0 acc 0]
      (if (>= i n)
        acc
        (let [bi (nth board i)
              acc2 (loop [j (inc i) a acc]
                     (if (>= j n)
                       a
                       (let [bj (nth board j)
                             diff (- bi bj)
                             adiff (if (neg? diff) (- diff) diff)]
                         (recur (inc j)
                                (if (or (= bi bj) (= adiff (- j i)))
                                  (inc a)
                                  a)))))]
          (recur (inc i) acc2))))))

(defn delta-attacks [board n r old-c new-c]
  (loop [i 0 d 0]
    (if (>= i n)
      d
      (if (= i r)
        (recur (inc i) d)
        (let [bi (nth board i)
              dist (let [x (- i r)] (if (neg? x) (- x) x))
              old-d (let [x (- bi old-c)] (if (neg? x) (- x) x))
              new-d (let [x (- bi new-c)] (if (neg? x) (- x) x))
              d1 (if (or (= bi old-c) (= old-d dist)) (dec d) d)
              d2 (if (or (= bi new-c) (= new-d dist)) (inc d1) d1)]
          (recur (inc i) d2))))))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)
(def ^:private two53-inv 1.1102230246251565E-16)

(defn lcg-next [s]
  (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))

(defn anneal [board seed]
  (let [n (count board)
        init-c (attacking board)]
    (loop [b board cur init-c best board bestc init-c
           temp 100.0 rng seed steps 0]
      (if (or (<= temp 0.01) (>= steps 5000))
        [best bestc]
        (let [rng1 (lcg-next rng)
              r (int (Long/remainderUnsigned rng1 n))
              rng2 (lcg-next rng1)
              c0 (int (Long/remainderUnsigned rng2 n))
              old (nth b r)
              c (if (= c0 old) (mod (inc c0) n) c0)
              delta (delta-attacks b n r old c)
              nc (+ cur delta)]
          (if (< delta 0)
            (let [nb (assoc b r c)
                  nbest (if (< nc bestc) nb best)
                  nbestc (if (< nc bestc) nc bestc)]
              (recur nb nc nbest nbestc (* temp 0.99) rng2 (inc steps)))
          (let [rng3 (lcg-next rng2)
                d (* (double (unsigned-bit-shift-right rng3 11)) two53-inv)]
            (if (< d (Math/exp (/ (- (double delta)) temp)))
              (let [nb (assoc b r c)
                    nbest (if (< nc bestc) nb best)
                    nbestc (if (< nc bestc) nc bestc)]
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
