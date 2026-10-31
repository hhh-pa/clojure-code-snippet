;; lrta_star: Learning Real-Time A* on a grid with Manhattan heuristic.
;; Mirrors the C++ port: H-table update H(s) = min(1 + H(s')), greedy move
;; with row/col tie-break, 10000-step cap. Outputs steps then flat path.
(ns lrta-star
  (:require [clojure.string :as str]))

(def ^:private dirs [[-1 0] [1 0] [0 -1] [0 1]])

(defn lrta [grid sr sc gr gc]
  (let [r (count grid)
        c (count (first grid))
        h-man (fn [cr cc] (+ (Math/abs (- cr gr)) (Math/abs (- cc gc))))]
    (loop [cr sr cc sc steps 0 path [[sr sc]] h {}]
      (if (or (and (= cr gr) (= cc gc)) (>= steps 10000))
        [steps path]
        (let [nbrs (filterv (fn [[nr nc]]
                              (and (>= nr 0) (< nr r) (>= nc 0) (< nc c)
                                   (zero? (get-in grid [nr nc]))))
                            (mapv (fn [[dr dc]] [(+ cr dr) (+ cc dc)]) dirs))]
          (if (empty? nbrs)
            [steps path]
            (let [get-h (fn [a b] (get h [a b] (h-man a b)))
                  min-h (apply min (mapv (fn [[nr nc]] (+ 1 (get-h nr nc))) nbrs))
                  h2 (assoc h [cr cc] min-h)
                  best (reduce (fn [b nb]
                                 (let [hv (+ 1 (get-h (first nb) (second nb)))
                                       bh (+ 1 (get-h (first b) (second b)))]
                                   (if (or (< hv bh)
                                           (and (= hv bh) (neg? (compare nb b))))
                                     nb
                                     b)))
                               (first nbrs) (rest nbrs))]
              (recur (first best) (second best) (inc steps)
                     (conj path best) h2))))))))

(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        nums (mapv #(Long/parseLong %) toks)
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [rr (int (nth nums pos))
              cc (int (nth nums (inc pos)))
              p1 (+ pos 2)
              grid (vec (for [i (range rr)]
                          (vec (for [j (range cc)]
                                 (int (nth nums (+ p1 (* i cc) j)))))))
              p2 (+ p1 (* rr cc))
              sr (int (nth nums p2)) sc (int (nth nums (inc p2)))
              gr (int (nth nums (+ p2 2))) gc (int (nth nums (+ p2 3)))
              [steps path] (lrta grid sr sc gr gc)]
          (.append sb (str steps))
          (.append sb "\n")
          (.append sb (str/join " " (mapcat identity path)))
          (.append sb "\n")
          (recur (+ p2 4) (dec tcases) sb))))))

(-main)
