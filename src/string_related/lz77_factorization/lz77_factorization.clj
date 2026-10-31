(ns solution
  (:require [clojure.string :as str]))

(defn -main []
  (let [input (slurp *in*)
        toks (filter seq (str/split input #"\s+"))
        s (first toks)]
    (when (and s (pos? (count s)))
      (let [n (count s) b (vec s)]
        (loop [i 0]
          (when (< i n)
            (let [[bl bo] (loop [j 0 bl 0 bo 0]
                            (if (>= j i) [bl bo]
                              (let [len (loop [l 0] (if (and (< (+ i l) n) (< (+ j l) n) (= (nth b (+ j l)) (nth b (+ i l)))) (recur (inc l)) l))]
                                (if (> len bl)
                                  (recur (inc j) len (- i j))
                                  (recur (inc j) bl bo)))))]
              (if (zero? bl)
                (do (println (str "0 0 " (nth b i))) (recur (inc i)))
                (let [nxt (if (< (+ i bl) n) (nth b (+ i bl)) \$)]
                  (println (str bo " " bl " " nxt))
                  (let [ni (+ i bl (if (< (+ i bl) n) 1 0))]
                    (when (< ni n) (recur ni))))))))))))

(-main)
