
;; q_learning: tabular Q-learning and SARSA with seeded epsilon-greedy
;; exploration on an explicit small MDP. Episodes start at state 0
;; with a fixed horizon; greedy ties prefer the smallest action.
(ns q-learning
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn fmt6 [x]
  (String/format java.util.Locale/US "%.6f" (into-array Object [(double x)])))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)
(def ^:private two53-inv 1.1102230246251565E-16)

(defn lcg-next [s] (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))
(defn lcg-int [s n]
  (let [ns (lcg-next s)] [ns (int (Long/remainderUnsigned ns n))]))
(defn lcg-double [s]
  (let [ns (lcg-next s)]
    [ns (* (double (unsigned-bit-shift-right ns 11)) two53-inv)]))

(defn egreedy [rng q s na eps]
  (let [[r2 u] (lcg-double rng)]
    (if (< u eps)
      (let [[r3 a] (lcg-int r2 na)] [r3 a])
      [r2 (first (reduce (fn [[ba bv] a]
                           (let [v (get-in q [s a])]
                             (if (> v bv) [a v] [ba bv])))
                         [0 (get-in q [s 0])] (range 1 na)))])))

(defn trans-step [rng tr s a]
  (let [[r2 u] (lcg-double rng)
        lst (get-in tr [s a])
        [nst rw _] (loop [ts lst c 0.0]
                     (if (empty? ts)
                       [(first (last lst)) (nth (last lst) 2) c]
                       (let [[s2 p r] (first ts)
                             c2 (+ c p)]
                         (if (< u c2) [s2 r c2] (recur (rest ts) c2)))))]
    [r2 nst rw]))

(defn greedy-pol [q]
  (mapv (fn [row]
          (first (reduce (fn [[ba bv] a]
                           (let [v (nth row a)]
                             (if (> v bv) [a v] [ba bv])))
                         [0 (first row)] (range 1 (count row)))))
        q))

(defn qlearn [ns na tr g al eps e l seed]
  (loop [i 0 rng seed q (vec (repeat ns (vec (repeat na 0.0))))]
    (if (= i e)
      q
      (let [[rng2 q2] (loop [j 0 s 0 r rng qq q]
                        (if (= j l)
                          [r qq]
                          (let [[r2 a] (egreedy r qq s na eps)
                                [r3 nst rw] (trans-step r2 tr s a)
                                mx (apply max (nth qq nst))]
                            (recur (inc j) nst r3
                                   (update-in qq [s a] + (* al (+ rw (* g mx) (- (get-in qq [s a])))))))))]
        (recur (inc i) rng2 q2)))))

(defn sarsa [ns na tr g al eps e l seed]
  (loop [i 0 rng seed q (vec (repeat ns (vec (repeat na 0.0))))]
    (if (= i e)
      q
      (let [[rng2 q2] (let [[r0 a0] (egreedy rng q 0 na eps)]
                        (loop [j 0 s 0 a a0 r r0 qq q]
                          (if (= j l)
                            [r qq]
                            (let [[r2 nst rw] (trans-step r tr s a)
                                  [r3 na2] (egreedy r2 qq nst na eps)]
                              (recur (inc j) nst na2 r3
                                     (update-in qq [s a] + (* al (+ rw (* g (get-in qq [nst na2]))
                                                                     (- (get-in qq [s a]))))))))))]
        (recur (inc i) rng2 q2)))))

(defn parse-case [nums pos]
  (let [ns (int (nth nums pos)) na (int (nth nums (+ pos 1)))]
    (loop [s 0 a 0 p (+ pos 2) tr (vec (repeat ns (vec (repeat na []))))]
      (if (= s ns)
        (let [fr (fn [q] (/ (double (nth nums q)) (double (nth nums (inc q)))))
              g (fr p) al (fr (+ p 2)) eps (fr (+ p 4))
              e (int (nth nums (+ p 6))) l (int (nth nums (+ p 7)))
              seed (nth nums (+ p 8))]
          [ns na tr g al eps e l seed (+ p 9)])
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
        (let [[ns na tr g al eps e l seed p2] (parse-case nums pos)
              qq (qlearn ns na tr g al eps e l seed)
              sq (sarsa ns na tr g al eps e l seed)]
          (.append sb "QLEARN\n")
          (doseq [row qq]
            (.append sb (str (str/join " " (map fmt6 row)) "\n")))
          (.append sb (str "QP " (str/join " " (greedy-pol qq)) "\n"))
          (.append sb "SARSA\n")
          (doseq [row sq]
            (.append sb (str (str/join " " (map fmt6 row)) "\n")))
          (.append sb (str "SP " (str/join " " (greedy-pol sq)) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
