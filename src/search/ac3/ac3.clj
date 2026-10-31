
;; ac3: AC-3 arc consistency for binary CSPs. Queue-based domain pruning
;; over directed arcs; ops are != == < > with flipped reverse arcs.
(ns ac3
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn holds* [a b op]
  (cond (= op 0) (not= a b) (= op 1) (= a b) (= op 2) (< a b) :else (> a b)))

(defn flipop* [op]
  (cond (= op 2) 3 (= op 3) 2 :else op))

(defn revise* [dom i j op]
  (let [di (nth dom i) dj (nth dom j)]
    (loop [xs di nd [] ch false]
      (if (empty? xs)
        [(assoc dom i nd) ch]
        (let [x (first xs)
              sup (some #(holds* x % op) dj)]
          (recur (rest xs) (if sup (conj nd x) nd) (or ch (not sup))))))))

(defn ac3 [dom nbr]
  (let [n (count dom)
        q0 (into clojure.lang.PersistentQueue/EMPTY
                 (for [i (range n) [j op] (nth nbr i)] [i j op]))]
    (loop [d dom q q0]
      (if (empty? q)
        [d true]
        (let [[i j op] (peek q)
              q2 (pop q)
              [d2 ch] (revise* d i j op)]
          (if (not ch)
            (recur d2 q2)
            (if (empty? (nth d2 i))
              [d2 false]
              (recur d2 (reduce (fn [qq [k o2]]
                                  (if (= k j) qq (conj qq [k i o2])))
                                q2 (nth nbr i))))))))))

(defn solve-one [nums pos]
  (let [n (int (nth nums pos))]
    (loop [i 0 p (inc pos) dom []]
      (if (= i n)
        (let [c (int (nth nums p))
              p2 (inc p)
              nbr (loop [k 0 pp p2 nb (vec (repeat n []))]
                    (if (= k c)
                      nb
                      (let [ii (int (nth nums pp)) jj (int (nth nums (+ pp 1)))
                            op (int (nth nums (+ pp 2)))]
                        (recur (inc k) (+ pp 3)
                               (-> nb
                                   (update ii conj [jj op])
                                   (update jj conj [ii (flipop* op)]))))))
              [d ok] (ac3 dom nbr)
              out (if (not ok)
                    "AC3 INCONSISTENT\n"
                    (str "AC3\n" (str/join "" (map #(str (str/join " " %) "\n") d))))]
          [out (+ p2 (* c 3))])
        (let [sz (int (nth nums p))
              vals (vec (sort (map int (subvec nums (inc p) (+ p 1 sz)))))]
          (recur (inc i) (+ p 1 sz) (conj dom vals)))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[out p2] (solve-one nums pos)]
          (.append sb out)
          (recur p2 (dec tcases) sb))))))

(-main)
