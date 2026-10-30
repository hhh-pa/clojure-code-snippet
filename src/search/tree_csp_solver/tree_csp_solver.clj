
;; tree_csp_solver: tree-structured CSPs via directed arc consistency.
;; Bottom-up pruning of each parent to values supported by the child,
;; then top-down assignment of smallest consistent values.
(ns tree-csp-solver
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn holds* [a b op]
  (cond (= op 0) (not= a b) (= op 1) (= a b) (= op 2) (< a b) :else (> a b)))

(defn solve-one [nums pos]
  (let [n (int (nth nums pos))
        p0 (inc pos)
        par (vec (map int (subvec nums p0 (+ p0 n))))
        p1 (+ p0 n)
        [dom p2] (loop [i 0 p p1 d []]
                   (if (= i n)
                     [d p]
                     (let [sz (int (nth nums p))
                           vals (vec (sort (map int (subvec nums (inc p) (+ p 1 sz)))))]
                       (recur (inc i) (+ p 1 sz) (conj d vals)))))
        [op p3] (loop [i 0 p p2 o (vec (repeat n -1))]
                  (if (= i n)
                    [o p]
                    (if (neg? (nth par i))
                      (recur (inc i) p o)
                      (recur (inc i) (inc p) (assoc o i (int (nth nums p)))))))
        kids (loop [i 0 k (vec (repeat n []))]
               (if (= i n) k
                   (if (neg? (nth par i))
                     (recur (inc i) k)
                     (recur (inc i) (update k (nth par i) conj i)))))
        root (first (for [i (range n) :when (neg? (nth par i))] i))
        order (loop [st [root] ord []]
                (if (empty? st)
                  ord
                  (let [v (peek st)]
                    (recur (into (pop st) (nth kids v)) (conj ord v)))))
        [dom2 ok1] (loop [oi (dec (count order)) d dom]
                     (if (< oi 0)
                       [d true]
                       (let [v (nth order oi)]
                         (if (neg? (nth par v))
                           (recur (dec oi) d)
                           (let [pp (nth par v)
                                 nd (filterv (fn [x] (some #(holds* x % (nth op v)) (nth d v)))
                                             (nth d pp))]
                             (if (empty? nd)
                               [(assoc d pp nd) false]
                               (recur (dec oi) (assoc d pp nd))))))))]
    (if (not ok1)
      ["TREECSP INCONSISTENT\n" p3]
      (loop [oi 0 asn (vec (repeat n -1))]
        (if (= oi (count order))
          [(str "TREECSP " (str/join " " asn) "\n") p3]
          (let [v (nth order oi)]
            (if (neg? (nth par v))
              (if (empty? (nth dom2 v))
                ["TREECSP INCONSISTENT\n" p3]
                (recur (inc oi) (assoc asn v (first (nth dom2 v)))))
              (let [pv (nth asn (nth par v))
                    pick (first (filter #(holds* pv % (nth op v)) (nth dom2 v)))]
                (if (nil? pick)
                  ["TREECSP INCONSISTENT\n" p3]
                  (recur (inc oi) (assoc asn v pick)))))))))))

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
