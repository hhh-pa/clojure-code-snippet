;; birkhoff-vn: Birkhoff-von Neumann decomposition of scaled
;; doubly-stochastic matrices. Output: coeff + permutation per line.
(ns birkhoff-vn (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn kuhn [u adj matchR seen]
  (loop [vs (nth adj u) matchR matchR seen seen]
    (if (empty? vs) [false matchR seen]
      (let [v (first vs)]
        (if (nth seen v) (recur (rest vs) matchR seen)
          (let [seen2 (assoc seen v true)]
            (if (neg? (nth matchR v)) [true (assoc matchR v u) seen2]
              (let [[ok m2 s3] (kuhn (nth matchR v) adj matchR seen2)]
                (if ok [true (assoc m2 v u) s3]
                  (recur (rest vs) matchR seen2))))))))))
(defn full-match [n adj]
  (let [matchR (loop [u 0 matchR (vec (repeat n -1))]
                 (if (>= u n) matchR
                   (let [[_ m2 _] (kuhn u adj matchR (vec (repeat n false)))]
                     (recur (inc u) m2))))]
    (loop [v 0 ml (vec (repeat n 0))]
      (if (>= v n) ml
        (if (neg? (nth matchR v)) (recur (inc v) ml)
          (recur (inc v) (assoc ml (nth matchR v) v)))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            n (nth vals 0)
            A (vec (for [i (range n)] (vec (for [j (range n)] (nth vals (+ 2 (* i n) j))))))
            terms (loop [A A terms []]
                    (if (every? zero? (mapcat identity A)) terms
                      (let [adj (mapv (fn [i] (vec (filter #(pos? (nth (nth A i) %)) (range n)))) (range n))
                            ml (full-match n adj)
                            c (apply min (map (fn [i] (nth (nth A i) (nth ml i))) (range n)))
                            A2 (loop [i 0 A A]
                                 (if (>= i n) A
                                   (recur (inc i) (assoc-in A [i (nth ml i)] (- (nth (nth A i) (nth ml i)) c)))))]
                        (recur A2 (conj terms [c ml])))))]
        (println (count terms))
        (doseq [[c perm] terms]
          (println (str/join " " (map str (cons c perm)))))))))
(-main)
