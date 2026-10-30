;; derangements-rook: derangement numbers and rook polynomials.
;; Modes D n / R rows cols board.
(ns derangements-rook (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn derange [n]
  (cond (zero? n) 1 (= n 1) 0
        :else (loop [i 2 a 1 b 0]
                (let [c (* (dec i) (+ a b))]
                  (if (>= i n) c (recur (inc i) b c))))))
(defn rook-nums [board rows cols]
  (let [dp (loop [i 0 dp {0 1}]
             (if (>= i rows) dp
               (recur (inc i)
                 (reduce (fn [ndp [mask ways]]
                           (reduce (fn [ndp2 j]
                                     (if (and (= 1 (nth (nth board i) j)) (zero? (bit-and mask (bit-shift-left 1 j))))
                                       (let [nm (bit-or mask (bit-shift-left 1 j))]
                                         (assoc ndp2 nm (+ (get ndp2 nm 0) ways)))
                                       ndp2))
                                   ndp (range cols)))
                         dp dp))))]
    (mapv (fn [k] (reduce + 0 (map second (filter #(= k (count (filter (fn [j] (pos? (bit-and (first %) (bit-shift-left 1 j)))) (range cols)))) dp))))
          (range (inc (min rows cols))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "D" (nth toks 0))
        (println (derange (as-long (nth toks 1))))
        (let [rows (as-long (nth toks 1)) cols (as-long (nth toks 2))
              vals (mapv as-long (drop 3 toks))
              board (vec (for [i (range rows)] (vec (for [j (range cols)] (nth vals (+ (* i cols) j))))))]
          (println (str/join " " (map str (rook-nums board rows cols)))))))))
(-main)
