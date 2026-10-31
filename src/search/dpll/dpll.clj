
;; dpll: DPLL SAT solver with unit propagation and pure-literal
;; elimination. Branches on the smallest unassigned variable, trying
;; true first; unassigned variables print as 0.
(ns dpll
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn lit-true? [l asn]
  (let [x (dec (Math/abs (int l))) v (nth asn x)]
    (and (not= v -1) (= (= v 1) (pos? l)))))

(defn propagate [v clauses asn]
  (loop [a asn]
    (let [[a2 ch1 fail1]
          (reduce (fn [[aa ch fl] c]
                    (if fl
                      [aa ch fl]
                      (let [sat (some #(lit-true? % aa) c)]
                        (if sat
                          [aa ch fl]
                          (let [un (filterv #(= -1 (nth aa (dec (Math/abs (int %))))) c)]
                            (cond (empty? un) [aa ch true]
                                  (= 1 (count un))
                                  (let [u (first un) x (dec (Math/abs (int u)))
                                        val (if (pos? u) 1 0)]
                                    (cond (= -1 (nth aa x)) [(assoc aa x val) true fl]
                                          (= (nth aa x) val) [aa ch fl]
                                          :else [aa ch true]))
                                  :else [aa ch fl]))))))
                  [a false false] clauses)]
      (if fail1
        [a2 true true]
        (let [unsat-cls (remove #(some (fn [l] (lit-true? l a2)) %) clauses)
              occ (reduce (fn [o c]
                            (reduce (fn [oo l]
                                      (let [x (dec (Math/abs (int l)))]
                                        (if (= -1 (nth a2 x))
                                          (update oo x (fnil conj #{}) (pos? l))
                                          oo)))
                                    o c))
                          (vec (repeat v #{})) unsat-cls)
              [a3 ch2] (reduce (fn [[aa ch] x]
                                 (if (not= -1 (nth aa x))
                                   [aa ch]
                                   (let [s (nth occ x)]
                                     (cond (= s #{true}) [(assoc aa x 1) true]
                                           (= s #{false}) [(assoc aa x 0) true]
                                           :else [aa ch]))))
                               [a2 false] (range v))]
          (if (or ch1 ch2)
            (recur a3)
            [a3 false false]))))))

(declare dpll*)

(defn dpll* [v clauses asn]
  (let [[a _ fail] (propagate v clauses asn)]
    (if fail
      nil
      (let [bad (some (fn [c]
                        (let [sat (some #(lit-true? % a) c)
                              un (some #(= -1 (nth a (dec (Math/abs (int %))))) c)]
                          (and (not sat) (not un))))
                      clauses)]
        (if bad
          nil
          (let [br (first (for [x (range v) :when (= -1 (nth a x))] x))]
            (if (nil? br)
              a
              (or (dpll* v clauses (assoc a br 1))
                  (dpll* v clauses (assoc a br 0))))))))))

(defn parse-case [nums pos]
  (let [v (int (nth nums pos)) c (int (nth nums (+ pos 1)))]
    (loop [i 0 p (+ pos 2) cls []]
      (if (= i c)
        [v cls p]
        (let [k (int (nth nums p))]
          (recur (inc i) (+ p 1 k) (conj cls (vec (map int (subvec nums (inc p) (+ p 1 k)))))))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[v cls p] (parse-case nums pos)
              res (dpll* v cls (vec (repeat v -1)))]
          (if (nil? res)
            (.append sb "DPLL UNSAT\n")
            (.append sb (str "DPLL SAT " (str/join " " (map #(if (= % -1) 0 %) res)) "\n")))
          (recur p (dec tcases) sb))))))

(-main)
