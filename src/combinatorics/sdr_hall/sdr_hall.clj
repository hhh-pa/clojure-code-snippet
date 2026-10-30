;; sdr-hall: systems of distinct representatives via Kuhn matching.
;; Input: n m e then edges. Output: matching or NO.
(ns sdr-hall (:require [clojure.string :as str]))
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
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [vals (mapv parse-token toks)
            n (nth vals 0) m (nth vals 1) e (nth vals 2)
            adj (loop [i 0 a (vec (repeat n []))]
                  (if (>= i e) (mapv sort a)
                    (let [u (nth vals (+ 3 (* 2 i))) v (nth vals (+ 4 (* 2 i)))]
                      (recur (inc i) (assoc a u (conj (nth a u) v))))))
            matchR (loop [u 0 matchR (vec (repeat m -1))]
                     (if (>= u n) matchR
                       (let [[_ m2 _] (kuhn u adj matchR (vec (repeat m false)))]
                         (recur (inc u) m2))))
            matchL (loop [v 0 ml (vec (repeat n -1))]
                     (if (>= v m) ml
                       (if (neg? (nth matchR v)) (recur (inc v) ml)
                         (recur (inc v) (assoc ml (nth matchR v) v)))))]
        (if (some neg? matchL) (println "NO")
          (doseq [u (range n)] (println u (nth matchL u))))))))
(-main)
