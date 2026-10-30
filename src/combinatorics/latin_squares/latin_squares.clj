;; latin-squares: orthogonal Latin squares (prime order) and
;; Euler's 36 officers (n=6 has no orthogonal pair).
(ns latin-squares (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn prime? [n]
  (if (< n 2) false
    (loop [d 2] (if (> (* d d) n) true (if (zero? (mod n d)) false (recur (inc d)))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (let [n (parse-token (nth toks 0))]
        (if (= n 6) (println "NONE")
          (do (doseq [i (range n)]
                (println (str/join " " (map str (map #(mod (+ i %) n) (range n))))))
              (if (prime? n)
                (do (doseq [i (range n)]
                      (println (str/join " " (map str (map #(mod (+ i (* 2 %)) n) (range n))))))
                    (println "ORTHOGONAL"))
                (println "NO_PAIR"))))))))
(-main)
