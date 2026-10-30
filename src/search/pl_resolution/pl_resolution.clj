
;; pl_resolution: propositional resolution refutation. Adds the negated
;; query to the CNF knowledge base and derives resolvents pair by pair
;; until the empty clause appears or saturation.
(ns pl-resolution
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn norm-clause [c] (vec (distinct (sort c))))

(defn taut? [c]
  (let [s (set c)]
    (some #(contains? s (- %)) c)))

(defn resolvents [ci cj]
  (vec (for [l1 ci l2 cj :when (= l1 (- l2))]
         (norm-clause (concat (remove #(= % l1) ci) (remove #(= % l2) cj))))))

(defn solve-one [clauses]
  (loop [cls clauses round 0]
    (if (>= round 500)
      false
      (let [n (count cls)
            res (vec (for [i (range n) j (range (inc i) n)
                           r (resolvents (nth cls i) (nth cls j))] r))]
        (if (some empty? res)
          true
          (let [added (vec (distinct (remove (fn [r] (or (taut? r) (some #(= % r) cls))) res)))]
            (if (empty? added)
              false
              (let [cls2 (into cls added)]
                (if (> (count cls2) 4000) false (recur cls2 (inc round)))))))))))

(defn parse-case [nums pos]
  (let [v (int (nth nums pos)) c (int (nth nums (+ pos 1)))]
    (loop [i 0 p (+ pos 2) cls []]
      (if (= i c)
        [cls p]
        (let [k (int (nth nums p))
              cl (norm-clause (mapv int (subvec nums (inc p) (+ p 1 k))))]
          (recur (inc i) (+ p 1 k) (if (taut? cl) cls (conj cls cl))))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[cls p] (parse-case nums pos)
              q (int (nth nums p))
              nq [(- q)]
              cls2 (if (some #(= % nq) cls) cls (conj cls nq))]
          (.append sb (if (solve-one cls2) "RESOLUTION ENTAILED\n" "RESOLUTION NOT-ENTAILED\n"))
          (recur (inc p) (dec tcases) sb))))))

(-main)
