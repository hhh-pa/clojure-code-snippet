;; prufer: Prufer code bijection between labeled trees and sequences.
;; Modes E n edges / D n sequence. Labels are 0-based.
(ns prufer (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn min-leaf [in-leaves]
  (first (filter #(nth in-leaves %) (range (count in-leaves)))))
(defn prufer-encode [n edges]
  (let [adj (loop [i 0 a (vec (repeat n #{}))]
              (if (>= i (count edges)) a
                (let [[u v] (nth edges i)]
                  (recur (inc i) (assoc a u (conj (nth a u) v) v (conj (nth a v) u))))))
        deg (mapv count adj)]
    (loop [i 0 adj adj deg deg inl (mapv #(= 1 %) deg) acc []]
      (if (>= i (- n 2)) acc
        (let [leaf (min-leaf inl)
              nb (first (sort (nth adj leaf)))
              adj2 (assoc adj leaf (disj (nth adj leaf) nb) nb (disj (nth adj nb) leaf))
              deg2 (assoc deg nb (dec (nth deg nb)))]
          (recur (inc i) adj2 deg2
                 (assoc inl leaf false nb (= 1 (nth deg2 nb)))
                 (conj acc nb)))))))
(defn prufer-decode [n sq]
  (let [deg (loop [i 0 d (vec (repeat n 1))]
              (if (>= i (count sq)) d (recur (inc i) (assoc d (nth sq i) (inc (nth d (nth sq i)))))))]
    (loop [i 0 deg deg inl (mapv #(= 1 %) deg) edges []]
      (if (>= i (count sq))
        (let [a (min-leaf inl)
              inl2 (assoc inl a false)
              b (min-leaf inl2)]
          (sort (conj edges [(min a b) (max a b)])))
        (let [v (nth sq i) leaf (min-leaf inl)
              deg2 (assoc deg v (dec (nth deg v)))]
          (recur (inc i) deg2
                 (assoc inl leaf false v (= 1 (nth deg2 v)))
                 (conj edges [(min leaf v) (max leaf v)])))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "E" (nth toks 0))
        (let [n (as-long (nth toks 1))
              edges (vec (for [i (range (dec n))] [(as-long (nth toks (+ 2 (* 2 i)))) (as-long (nth toks (+ 3 (* 2 i))))]))]
          (println (str/join " " (map str (prufer-encode n edges)))))
        (let [n (as-long (nth toks 1))
              sq (mapv as-long (take (- n 2) (drop 2 toks)))]
          (doseq [[u v] (prufer-decode n sq)] (println u v)))))))
(-main)
