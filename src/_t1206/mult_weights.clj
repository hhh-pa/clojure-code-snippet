;; mult-weights: multiplicative weights update for gains.
;; Input: n T eta then T gain rows. Output: final distribution %.6f.
(ns mult-weights (:require [clojure.string :as str]))
(def ^:private US java.util.Locale/US)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Double/parseDouble t))
(defn fmt [x] (String/format US "%.6f" (into-array Object [(+ (double x) 0.0)])))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [n (long (Double/parseDouble (nth toks 0)))
            T (long (Double/parseDouble (nth toks 1)))
            eta (parse-token (nth toks 2))
            vals (mapv parse-token (drop 3 toks))
            g (vec (for [tt (range T)] (vec (for [i (range n)] (nth vals (+ (* tt n) i))))))
            w (loop [tt 0 w (vec (repeat n 1.0))]
                (if (>= tt T) w
                  (recur (inc tt) (mapv (fn [i] (* (nth w i) (+ 1 (* eta (nth (nth g tt) i))))) (range n)))))
            s (reduce + w)]
        (println (str/join " " (map (fn [v] (fmt (/ v s))) w)))))))
(-main)
