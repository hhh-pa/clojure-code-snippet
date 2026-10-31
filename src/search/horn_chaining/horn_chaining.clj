
;; horn_chaining: Horn-clause entailment by forward chaining (linear
;; data-driven inference with premise counters) and backward chaining
;; (goal-directed DFS with loop detection).
(ns horn-chaining
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn forward-chain [v prem concl byprem q]
  (let [r (count prem)
        cnt (mapv count prem)
        inferred0 (vec (repeat v false))
        [inf0 ag0] (reduce (fn [[inf ag] i]
                             (if (and (zero? (nth cnt i)) (not (nth inf (nth concl i))))
                               [(assoc inf (nth concl i) true) (conj ag (nth concl i))]
                               [inf ag]))
                           [inferred0 clojure.lang.PersistentQueue/EMPTY] (range r))]
    (loop [cnt cnt inf inf0 ag ag0]
      (if (empty? ag)
        false
        (let [x (peek ag) ag2 (pop ag)]
          (if (= x q)
            true
            (let [[cnt2 inf2 ag3]
                  (reduce (fn [[cc ii aa] ri]
                            (let [nc (dec (nth cc ri))]
                              (if (zero? nc)
                                (let [c (nth concl ri)]
                                  (if (nth ii c)
                                    [(assoc cc ri nc) ii aa]
                                    [(assoc cc ri nc) (assoc ii c true) (conj aa c)]))
                                [(assoc cc ri nc) ii aa])))
                          [cnt inf ag2] (nth byprem x))]
              (recur cnt2 inf2 ag3))))))))

(declare prove*)

(defn prove* [g prem byconcl isfact proved visiting]
  (cond (nth proved g) [true proved]
        (nth visiting g) [false proved]
        (nth isfact g) [true (assoc proved g true)]
        :else
        (loop [ris (nth byconcl g) pv proved]
          (if (empty? ris)
            [false pv]
            (let [[ok pv2] (loop [prs (nth prem (first ris)) pvx pv]
                             (if (empty? prs)
                               [true pvx]
                               (let [[ok2 pv3] (prove* (first prs) prem byconcl isfact pvx
                                                       (assoc visiting g true))]
                                 (if ok2
                                   (recur (rest prs) pv3)
                                   [false pv3]))))]
              (if ok
                [true (assoc pv2 g true)]
                (recur (rest ris) pv2)))))))

(defn backward-chain [prem byconcl isfact q]
  (let [v (count isfact)]
    (first (prove* q prem byconcl isfact (vec (repeat v false)) (vec (repeat v false))))))

(defn parse-case [nums pos]
  (let [v (int (nth nums pos)) r (int (nth nums (+ pos 1)))]
    (loop [i 0 p (+ pos 2) prem [] concl [] byprem (vec (repeat v []))]
      (if (= i r)
        [v prem concl byprem p]
        (let [k (int (nth nums p))
              pr (vec (map int (subvec nums (inc p) (+ p 1 k))))
              c (int (nth nums (+ p 1 k)))
              bp2 (reduce (fn [bp x] (update bp x conj i)) byprem pr)]
          (recur (inc i) (+ p 2 k) (conj prem pr) (conj concl c) bp2))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[v prem concl byprem p] (parse-case nums pos)
              q (int (nth nums p))
              fc (forward-chain v prem concl byprem q)
              byconcl (loop [i 0 bc (vec (repeat v []))]
                        (if (= i (count prem)) bc (recur (inc i) (update bc (nth concl i) conj i))))
              isfact (loop [i 0 f (vec (repeat v false))]
                       (if (= i (count prem)) f
                           (recur (inc i) (if (empty? (nth prem i)) (assoc f (nth concl i) true) f))))
              bc (backward-chain prem byconcl isfact q)]
          (.append sb (str "HORN FC:" (if fc "YES" "NO") " BC:" (if bc "YES" "NO") "\n"))
          (recur (inc p) (dec tcases) sb))))))

(-main)
