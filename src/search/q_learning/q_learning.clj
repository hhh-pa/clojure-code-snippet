
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

(defn best-action [q base na]
  (loop [a 1 ba 0 bv (nth q base)]
    (if (>= a na)
      ba
      (let [v (nth q (+ base a))]
        (if (> v bv)
          (recur (inc a) a v)
          (recur (inc a) ba bv))))))

(defn max-q [q base na]
  (loop [a 1 bv (nth q base)]
    (if (>= a na)
      bv
      (let [v (nth q (+ base a))]
        (recur (inc a) (if (> v bv) v bv))))))

(defn egreedy [rng q s na eps]
  (let [[r2 u] (lcg-double rng)]
    (if (< u eps)
      (lcg-int r2 na)
      [r2 (best-action q (* s na) na)])))

(defn trans-step [rng tr s na a]
  (let [[r2 u] (lcg-double rng)
        lst (nth tr (+ (* s na) a))
        n (count lst)]
    (loop [i 0 c 0.0]
      (if (>= i n)
        (let [last-item (nth lst (dec n))]
          [r2 (nth last-item 0) (nth last-item 2)])
        (let [item (nth lst i)
              c2 (+ c (nth item 1))]
          (if (< u c2)
            [r2 (nth item 0) (nth item 2)]
            (recur (inc i) c2)))))))

(defn unflatten-q [q ns na]
  (mapv (fn [s]
          (mapv (fn [a] (nth q (+ (* s na) a))) (range na)))
        (range ns)))

(defn greedy-pol [q]
  (mapv (fn [row]
          (first (reduce (fn [[ba bv] a]
                           (let [v (nth row a)]
                             (if (> v bv) [a v] [ba bv])))
                         [0 (first row)] (range 1 (count row)))))
        q))

(defn qlearn [ns na tr g al eps e l seed]
  (let [q (transient (vec (repeat (* ns na) 0.0)))]
    (loop [i 0 rng seed]
      (if (= i e)
        (unflatten-q (persistent! q) ns na)
        (let [rng2 (loop [j 0 s 0 r rng]
                     (if (= j l)
                       r
                       (let [[r2 a] (egreedy r q s na eps)
                             [r3 nst rw] (trans-step r2 tr s na a)
                             mx (max-q q (* nst na) na)
                             idx (+ (* s na) a)
                             old-q (nth q idx)]
                         (assoc! q idx (+ old-q (* al (+ rw (* g mx) (- old-q)))))
                         (recur (inc j) nst r3))))]
          (recur (inc i) rng2))))))

(defn sarsa [ns na tr g al eps e l seed]
  (let [q (transient (vec (repeat (* ns na) 0.0)))]
    (loop [i 0 rng seed]
      (if (= i e)
        (unflatten-q (persistent! q) ns na)
        (let [[r0 a0] (egreedy rng q 0 na eps)
              rng2 (loop [j 0 s 0 a a0 r r0]
                     (if (= j l)
                       r
                       (let [[r2 nst rw] (trans-step r tr s na a)
                             [r3 na2] (egreedy r2 q nst na eps)
                             idx (+ (* s na) a)
                             old-q (nth q idx)
                             nxt-q (nth q (+ (* nst na) na2))]
                         (assoc! q idx (+ old-q (* al (+ rw (* g nxt-q) (- old-q)))))
                         (recur (inc j) nst na2 r3))))]
          (recur (inc i) rng2))))))

(defn parse-case [nums pos]
  (let [ns (int (nth nums pos)) na (int (nth nums (+ pos 1)))]
    (loop [s 0 a 0 p (+ pos 2) tr (vec (repeat (* ns na) []))]
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
          (recur s2 a3 p2 (assoc tr (+ (* s na) a) lst)))))))

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
