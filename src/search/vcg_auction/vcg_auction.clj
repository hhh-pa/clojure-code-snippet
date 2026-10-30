
;; vcg_auction: Vickrey-Clarke-Groves combinatorial auction. Brute-force
;; welfare-maximizing winner determination plus Clarke-pivot payments
;; (each winner pays the externality imposed on the others).
(ns vcg-auction
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn welfare* [bb bmask bv b skip]
  (loop [m 0 best 0 bestm 0]
    (if (= m (bit-shift-left 1 b))
      [best bestm]
      (let [[used tot ok] (loop [i 0 u 0 t 0 o true]
                            (if (or (= i b) (not o))
                              [u t o]
                              (if (zero? (bit-and (bit-shift-right m i) 1))
                                (recur (inc i) u t o)
                                (if (or (and (>= skip 0) (= (nth bb i) skip))
                                        (pos? (bit-and u (nth bmask i))))
                                  [u t false]
                                  (recur (inc i) (bit-or u (nth bmask i)) (+ t (nth bv i)) true)))))]
        (recur (inc m) (if (and ok (> tot best)) tot best) (if (and ok (> tot best)) m bestm))))))

(defn solve-one [n bids]
  (let [b (count bids)
        bb (mapv first bids) bmask (mapv second bids) bv (mapv #(nth % 2) bids)
        [w bestm] (welfare* bb bmask bv b -1)
        won (vec (for [i (range b) :when (= 1 (bit-and (bit-shift-right bestm i) 1))] i))
        pays (vec (for [i (range n)]
                    (let [mine (reduce + 0 (for [x won :when (= (nth bb x) i)] (nth bv x)))
                          [wo _] (welfare* bb bmask bv b i)]
                      (- wo (- w mine)))))]
    [w pays]))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos)) m (int (nth nums (+ pos 1))) b (int (nth nums (+ pos 2)))]
    (loop [i 0 p (+ pos 3) bids []]
      (if (= i b)
        [n bids p]
        (let [bi (int (nth nums p)) k (int (nth nums (inc p)))
              mask (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums (+ p 2) (+ p 2 k))))
              v (nth nums (+ p 2 k))]
          (recur (inc i) (+ p 3 k) (conj bids [bi mask v])))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n bids p2] (parse-case nums pos)
              [w pays] (solve-one n bids)]
          (.append sb (str "VCG " w "\n"))
          (.append sb (str "PAY " (str/join " " pays) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
