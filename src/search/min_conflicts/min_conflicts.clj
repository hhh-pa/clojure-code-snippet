
;; min_conflicts: N-queens min-conflicts local search. Each step repairs
;; the most-conflicted queen (ties: smallest row) to its min-conflicts
;; column (ties: smallest column); stops at zero conflicts or step cap.
(ns min-conflicts
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn conflicts-at [b row col]
  (let [n (count b)]
    (count (for [i (range n) :when (not= i row)
                 :when (let [bi (nth b i)]
                         (or (= bi col)
                             (= (Math/abs (- bi col)) (Math/abs (- i row)))))]
             1))))

(defn total-pairs [b]
  (let [n (count b)]
    (count (for [i (range n) j (range (inc i) n)
                 :when (let [bi (nth b i) bj (nth b j)]
                         (or (= bi bj)
                             (= (Math/abs (- bi bj)) (Math/abs (- i j)))))]
             1))))

(defn min-conflicts [b ms]
  (let [n (count b)]
    (loop [board b step 0]
      (if (>= step ms)
        board
        (let [[vr vc] (loop [i 0 br -1 bc -1]
                        (if (>= i n)
                          [br bc]
                          (let [c (conflicts-at board i (nth board i))]
                            (if (and (pos? c) (> c bc))
                              (recur (inc i) i c)
                              (recur (inc i) br bc)))))]
          (if (< vr 0)
            board
            (let [[bc _] (loop [c 0 bestc (nth board vr)
                                bestv (conflicts-at board vr (nth board vr))]
                           (if (>= c n)
                             [bestc bestv]
                             (let [v (conflicts-at board vr c)]
                               (if (< v bestv)
                                 (recur (inc c) c v)
                                 (recur (inc c) bestc bestv)))))]
              (recur (assoc board vr bc) (inc step)))))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (int (nth nums pos)) ms (int (nth nums (+ pos 1)))
              b (vec (map int (subvec nums (+ pos 2) (+ pos 2 n))))
              final (min-conflicts b ms)]
          (.append sb (str (str/join " " final) "\n" (total-pairs final) "\n"))
          (recur (+ pos 2 n) (dec tcases) sb))))))

(-main)
