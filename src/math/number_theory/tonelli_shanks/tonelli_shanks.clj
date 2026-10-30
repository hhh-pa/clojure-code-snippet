(ns tonelli-shanks (:require [clojure.string :as str]))
(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        a (Long/parseLong (nth toks 0)) p (Long/parseLong (nth toks 1))
        a (mod a p)]
    (cond (zero? a) (println "0")
          (= p 2) (println a)
          (not= 1 (modpow a (quot (dec p) 2) p)) (println "NONE")
          :else (let [[q s] (loop [q (dec p) s 0] (if (even? q) (recur (quot q 2) (inc s)) [q s]))]
                  (if (= s 1) (let [r (modpow a (quot (+ p 1) 4) p)] (println (min r (- p r))))
                    (let [z (loop [z 2] (if (= (dec p) (modpow z (quot (dec p) 2) p)) z (recur (inc z))))
                          c (modpow z q p) r (modpow a (quot (inc q) 2) p) t (modpow a q p)]
                      (loop [c c r r t t m s]
                        (if (= t 1) (println (min r (- p r)))
                          (let [i (loop [i 1 t2 (mod (* t t) p)] (if (= t2 1) i (recur (inc i) (mod (* t2 t2) p))))
                                b (modpow c (bit-shift-left 1 (- m i 1)) p)]
                            (recur (mod (* b b) p) (mod (* r b) p) (mod (* t b b) p) i))))))))))
(-main)
