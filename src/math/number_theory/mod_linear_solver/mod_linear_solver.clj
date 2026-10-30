(ns mod-linear-solver (:require [clojure.string :as str]))
(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        a (Long/parseLong (nth toks 0)) b (Long/parseLong (nth toks 1)) n (Long/parseLong (nth toks 2))]
    (cond (= n 1) (println "0")
          (<= n 0) (println "NONE")
          :else (let [a (mod a n) b (mod b n)
                      [g x _] (egcd a n)]
                  (if (not= 0 (mod b g)) (println "NONE")
                    (let [n2 (quot n g) x0 (mod (* x (quot b g)) n)
                          sols (sort (map #(mod (+ x0 (* % n2)) n) (range g)))]
                      (println g) (println (str/join " " sols))))))))
(-main)
