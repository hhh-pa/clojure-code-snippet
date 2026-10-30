(ns solution (:require [clojure.string :as str]))
(defn -main []
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+"))
        s1 (first toks) s2 (second toks)]
    (when (and s1 s2)
      (let [n (count s1) m (count s2) NEG -1000000000 MATCH 2 MISM -1 GO -2 GE -1
            M (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            Ix (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            Iy (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            M (assoc-in M [0 0] 0)
            Ix (reduce (fn [a i] (assoc-in a [i 0] (+ GO (* (dec i) GE)))) Ix (range 1 (inc n)))
            Iy (reduce (fn [a j] (assoc-in a [0 j] (+ GO (* (dec j) GE)))) Iy (range 1 (inc m)))
            [M Ix Iy] (reduce (fn [[M Ix Iy] [i j]]
                         (let [sc (if (= (nth s1 (dec i)) (nth s2 (dec j))) MATCH MISM)
                               best (max (get-in M [(dec i) (dec j)]) (get-in Ix [(dec i) (dec j)]) (get-in Iy [(dec i) (dec j)]))
                               nvM (+ best sc)
                               nvIx (max (+ (get-in M [(dec i) j]) GO) (+ (get-in Ix [(dec i) j]) GE))
                               nvIy (max (+ (get-in M [i (dec j)]) GO) (+ (get-in Iy [i (dec j)]) GE))]
                           [(assoc-in M [i j] nvM) (assoc-in Ix [i j] nvIx) (assoc-in Iy [i j] nvIy)]))
                       [M Ix Iy] (for [i (range 1 (inc n)) j (range 1 (inc m))] [i j]))]
        (println (max (get-in M [n m]) (get-in Ix [n m]) (get-in Iy [n m])))))))
(-main)
