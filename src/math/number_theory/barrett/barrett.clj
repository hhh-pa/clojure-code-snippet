;; barrett: Barrett reduction with mu = floor(2^32 / m).
;; Input: x m. Output: quotient and remainder of x / m.
(ns barrett (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            x (nth vals 0) m (nth vals 1)
            mu (quot (bit-shift-left 1 32) m)]
        (loop [q (bit-shift-right (* x mu) 32) r (- x (* (bit-shift-right (* x mu) 32) m))]
          (cond (neg? r) (recur (dec q) (+ r m))
                (>= r m) (recur (inc q) (- r m))
                :else (println q r)))))))
(-main)
