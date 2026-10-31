;; cantor-expansion: factoradic (Cantor) expansions and base-b arithmetic.
;; Modes E n perm / D n digits / A b X Y / M b X Y.
(ns cantor-expansion (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn fact [n] (loop [i 2 f 1] (if (> i n) f (recur (inc i) (* f i)))))
(defn dval [c] (Character/digit ^char c 36))
(defn dchar [v] (nth "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" v))
(defn stripz [s] (let [t (str/replace s #"^0+" "")] (if (str/blank? t) "0" t)))
(defn add-base [X Y b]
  (loop [i (dec (count X)) j (dec (count Y)) carry 0 acc ()]
    (if (and (neg? i) (neg? j) (zero? carry)) (stripz (apply str acc))
      (let [s (+ carry (if (neg? i) 0 (dval (nth X i))) (if (neg? j) 0 (dval (nth Y j))))]
        (recur (dec i) (dec j) (quot s b) (cons (dchar (mod s b)) acc))))))
(defn mul-base [X Y b]
  (let [X (stripz X) Y (stripz Y)]
    (if (or (= "0" X) (= "0" Y)) "0"
      (let [a (mapv dval (reverse X)) c (mapv dval (reverse Y))
            r (loop [i 0 r (vec (repeat (+ (count a) (count c)) 0))]
                (if (>= i (count a)) r
                  (recur (inc i) (loop [j 0 r r]
                                    (if (>= j (count c)) r
                                      (recur (inc j) (assoc r (+ i j) (+ (nth r (+ i j)) (* (nth a i) (nth c j))))))))))
            [r carry] (loop [i 0 r r carry 0]
                        (if (>= i (count r)) [r carry]
                          (let [t (+ (nth r i) carry)]
                            (recur (inc i) (assoc r i (mod t b)) (quot t b)))))
            digs (loop [r r carry carry]
                   (if (zero? carry) r (recur (conj r (mod carry b)) (quot carry b))))]
        (stripz (apply str (map dchar (reverse digs))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (cond (= "E" (nth toks 0))
            (let [n (as-long (nth toks 1))
                  p (mapv as-long (take n (drop 2 toks)))
                  L (mapv (fn [i] (count (filter #(> (nth p i) %) (subvec p (inc i))))) (range (dec n)))
                  rank (reduce + (map * L (map fact (map #(- n 1 %) (range (dec n))))))]
              (println (str/join " " (map str L)))
              (println rank))
            (= "D" (nth toks 0))
            (let [n (as-long (nth toks 1))
                  L (conj (vec (map as-long (take (dec n) (drop 2 toks)))) 0)]
              (println (str/join " " (map str
                (loop [i 0 avail (vec (range n)) acc []]
                  (if (>= i n) acc
                    (let [li (nth L i)]
                      (recur (inc i) (vec (concat (subvec avail 0 li) (subvec avail (inc li)))) (conj acc (nth avail li))))))))))
            (= "A" (nth toks 0))
            (println (add-base (nth toks 2) (nth toks 3) (as-long (nth toks 1))))
            :else
            (println (mul-base (nth toks 2) (nth toks 3) (as-long (nth toks 1))))))))
(-main)
