;; integer-partition: partition generation and Ferrers conjugate.
;; Modes G n / C k parts.
(ns integer-partition (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn gen-parts [rem maxv pre]
  (if (zero? rem) [(vec pre)]
    (mapcat (fn [v] (gen-parts (- rem v) v (conj pre v))) (range (min maxv rem) 0 -1))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "G" (nth toks 0))
        (let [n (as-long (nth toks 1))]
          (doseq [p (gen-parts n n [])]
            (println (str/join " " (map str p)))))
        (let [k (as-long (nth toks 1))
              p (mapv as-long (take k (drop 2 toks)))
              c (mapv (fn [j] (count (filter #(> % j) p))) (range (nth p 0)))]
          (println (str/join " " (map str c))))))))
(-main)
