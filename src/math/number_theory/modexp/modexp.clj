(ns modexp (:require [clojure.string :as str]))
(defn modpow [b e m]
  (if (= m 1) 0
    (loop [r (mod 1 m) b (mod b m) e e]
      (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2))))))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        vals (mapv #(Long/parseLong %) toks)]
    (cond (= 3 (count vals)) (println (modpow (nth vals 0) (nth vals 1) (nth vals 2)))
          :else (let [t (first vals)]
                  (if (and (pos? t) (= (count vals) (inc (* 3 t))))
                    (doseq [i (range t)] (println (modpow (nth vals (+ 1 (* 3 i))) (nth vals (+ 2 (* 3 i))) (nth vals (+ 3 (* 3 i))))))
                    (println (modpow (nth vals 0) (nth vals 1) (nth vals 2))))))))
(-main)
