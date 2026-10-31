
;; minimax_alphabeta: full minimax vs alpha-beta pruning on a complete
;; binary game tree (max at root, alternating). Reports the minimax value
;; plus leaf-visit counts for both, showing pruning savings.
(ns minimax-alphabeta
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(declare mm-full mm-ab)

(defn mm-full [leaves idx depth ismax cnt]
  (if (zero? depth)
    [(nth leaves idx) (inc cnt)]
    (let [[a c1] (mm-full leaves (* idx 2) (dec depth) (not ismax) cnt)
          [b c2] (mm-full leaves (inc (* idx 2)) (dec depth) (not ismax) c1)]
      [(if ismax (max a b) (min a b)) c2])))

(defn mm-ab [leaves idx depth ismax alpha beta cnt]
  (if (zero? depth)
    [(nth leaves idx) (inc cnt)]
    (if ismax
      (let [[r1 c1] (mm-ab leaves (* idx 2) (dec depth) false alpha beta cnt)
            v1 (max -2000000000 r1)
            a1 (max alpha v1)]
        (if (>= a1 beta)
          [v1 c1]
          (let [[r2 c2] (mm-ab leaves (inc (* idx 2)) (dec depth) false a1 beta c1)]
            [(max v1 r2) c2])))
      (let [[r1 c1] (mm-ab leaves (* idx 2) (dec depth) true alpha beta cnt)
            v1 (min 2000000000 r1)
            b1 (min beta v1)]
        (if (>= alpha b1)
          [v1 c1]
          (let [[r2 c2] (mm-ab leaves (inc (* idx 2)) (dec depth) true alpha b1 c1)]
            [(min v1 r2) c2]))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [d (int (nth nums pos))
              n (bit-shift-left 1 d)
              leaves (vec (map int (subvec nums (inc pos) (+ pos 1 n))))
              [v1 c1] (mm-full leaves 0 d true 0)
              [_ c2] (mm-ab leaves 0 d true -2000000000 2000000000 0)]
          (.append sb (str "MINIMAX " v1 " FULL " c1 " AB " c2 "\n"))
          (recur (+ pos 1 n) (dec tcases) sb))))))

(-main)
