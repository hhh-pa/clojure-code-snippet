;; max3cnf: derandomized MAX-3-CNF 8/7-approximation by conditional
;; expectations. Input: n m then clauses. Output: assignment, #satisfied.
(ns max3cnf (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn expected-of [clauses asg]
  (reduce +
    (map (fn [cl]
           (loop [i 0 sat false open 0]
             (if (or sat (>= i 3))
               (cond sat 1.0 (zero? open) 0.0 :else (- 1.0 (Math/pow 0.5 open)))
               (let [lit (nth cl i) v (Math/abs (long lit)) a (nth asg v -1)]
                 (if (>= a 0)
                   (recur (inc i) (= (pos? lit) (= a 1)) open)
                   (recur (inc i) sat (inc open)))))))
         clauses)))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            n (nth vals 0) m (nth vals 1)
            clauses (vec (for [i (range m)] (vec (for [j (range 3)] (nth vals (+ 2 (* i 3) j))))))
            asg (loop [v 1 asg (vec (repeat (inc n) -1))]
                  (if (> v n) asg
                    (let [e1 (expected-of clauses (assoc asg v 1))
                          e0 (expected-of clauses (assoc asg v 0))]
                      (recur (inc v) (assoc asg v (if (>= e1 e0) 1 0))))))
            sat (count (filter (fn [cl] (some (fn [lit] (= (pos? lit) (= 1 (nth asg (Math/abs (long lit)))))) cl)) clauses))]
        (println (str/join " " (map str (subvec asg 1))))
        (println sat)))))
(-main)
