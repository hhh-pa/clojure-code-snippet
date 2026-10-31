;; next-permutation: lexicographic next permutation and next
;; r-combination. Modes P n perm / C n r comb. NONE if last.
(ns next-permutation (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn next-perm [p]
  (let [n (count p)
        i (loop [i (- n 2)]
             (if (or (neg? i) (< (nth p i) (nth p (inc i)))) i (recur (dec i))))]
    (when-not (neg? i)
      (let [j (loop [j (dec n)]
                 (if (> (nth p j) (nth p i)) j (recur (dec j))))
            p2 (assoc p i (nth p j) j (nth p i))]
        (vec (concat (subvec p2 0 (inc i)) (reverse (subvec p2 (inc i)))))))))
(defn next-comb [n c]
  (let [r (count c)
        i (loop [i (dec r)]
             (if (or (neg? i) (not= (nth c i) (+ (- n r) i))) i (recur (dec i))))]
    (when-not (neg? i)
      (loop [j (inc i) c (assoc c i (inc (nth c i)))]
        (if (>= j r) c
          (recur (inc j) (assoc c j (inc (nth c (dec j))))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "P" (nth toks 0))
        (let [n (as-long (nth toks 1))
              p (mapv as-long (take n (drop 2 toks)))
              nx (next-perm p)]
          (if (nil? nx) (println "NONE") (println (str/join " " (map str nx)))))
        (let [n (as-long (nth toks 1)) r (as-long (nth toks 2))
              c (mapv as-long (take r (drop 3 toks)))
              nx (next-comb n c)]
          (if (nil? nx) (println "NONE") (println (str/join " " (map str nx)))))))))
(-main)
