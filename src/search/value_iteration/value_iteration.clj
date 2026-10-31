
;; value_iteration: MDP value iteration with Bellman backups. Runs K
;; synchronous sweeps from zero utilities, then reads off the greedy
;; policy (ties prefer the smallest action).
(ns value-iteration
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn fmt6 [x]
  (String/format java.util.Locale/US "%.6f" (into-array Object [(double x)])))

(defn qval [trs u g]
  (reduce + 0.0 (map (fn [[s2 p r]] (* p (+ r (* g (nth u s2))))) trs)))

(defn solve-one [ns na tr g k]
  (let [u (loop [i 0 u (vec (repeat ns 0.0))]
            (if (= i k)
              u
              (recur (inc i) (vec (for [s (range ns)]
                                    (apply max (for [a (range na)]
                                                 (qval (nth (nth tr s) a) u g))))))))]
    [u (vec (for [s (range ns)]
              (first (reduce (fn [[ba bv] a]
                               (let [v (qval (nth (nth tr s) a) u g)]
                                 (if (> v bv) [a v] [ba bv])))
                             [0 -1e300] (range na)))))]))

(defn parse-case [nums pos]
  (let [ns (int (nth nums pos)) na (int (nth nums (+ pos 1)))]
    (loop [s 0 a 0 p (+ pos 2) tr (vec (repeat ns (vec (repeat na []))))]
      (if (= s ns)
        (let [g (/ (double (nth nums p)) (double (nth nums (inc p))))
              k (int (nth nums (+ p 2)))]
          [ns na tr g k (+ p 3)])
        (let [nt (int (nth nums p))
              p1 (inc p)
              lst (vec (for [i (range nt)]
                         [(int (nth nums (+ p1 (* i 4))))
                          (/ (double (nth nums (+ p1 (* i 4) 1))) (double (nth nums (+ p1 (* i 4) 2))))
                          (double (nth nums (+ p1 (* i 4) 3)))]))
              p2 (+ p1 (* nt 4))
              na2 (inc a)
              s2 (if (= na2 na) (inc s) s)
              a3 (if (= na2 na) 0 na2)]
          (recur s2 a3 p2 (assoc-in tr [s a] lst)))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[ns na tr g k p2] (parse-case nums pos)
              [u pol] (solve-one ns na tr g k)]
          (.append sb "VI\n")
          (.append sb (str (str/join " " (map fmt6 u)) "\n"))
          (.append sb (str (str/join " " pol) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
