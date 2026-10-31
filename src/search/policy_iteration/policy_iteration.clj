
;; policy_iteration: MDP policy iteration. Alternates truncated policy
;; evaluation (K sweeps) with greedy policy improvement until the
;; policy stabilizes; ties keep the current action.
(ns policy-iteration
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
  (loop [round 0 u (vec (repeat ns 0.0)) pol (vec (repeat ns 0))]
    (if (>= round 100)
      [u pol]
      (let [u2 (loop [e 0 uu u]
                 (if (= e k)
                   uu
                   (recur (inc e) (vec (for [s (range ns)]
                                         (qval (nth (nth tr s) (nth pol s)) uu g))))))
            [pol2 stable] (loop [s 0 pp pol ok true]
                            (if (= s ns)
                              [pp ok]
                              (let [[ba _] (reduce (fn [[ba bv] a]
                                                      (let [v (qval (nth (nth tr s) a) u2 g)]
                                                        (if (> v (+ bv 1e-12)) [a v] [ba bv])))
                                                    [(nth pp s) (qval (nth (nth tr s) (nth pp s)) u2 g)] (range na))]
                                (recur (inc s) (assoc pp s ba) (and ok (= ba (nth pp s)))))))]
        (if stable [u2 pol2] (recur (inc round) u2 pol2))))))

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
          (.append sb "PI\n")
          (.append sb (str (str/join " " (map fmt6 u)) "\n"))
          (.append sb (str (str/join " " pol) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
