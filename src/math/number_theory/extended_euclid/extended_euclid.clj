(ns extended-euclid (:require [clojure.string :as str]))
(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))]
      [g y1 (- x1 (* (quot a b) y1))])))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        a (Long/parseLong (nth toks 0)) b (Long/parseLong (nth toks 1))]
    (if (and (zero? a) (zero? b)) (println "0 0 0")
      (let [aa (Math/abs a) bb (Math/abs b)
            [g x y] (egcd aa bb)
            x (if (neg? a) (- x) x) y (if (neg? b) (- y) y)]
        (println g x y)))))
(-main)
