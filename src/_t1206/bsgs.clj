(ns bsgs (:require [clojure.string :as str]))
(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        g (mod (Long/parseLong (nth toks 0)) (Long/parseLong (nth toks 2)))
        h (mod (Long/parseLong (nth toks 1)) (Long/parseLong (nth toks 2)))
        p (Long/parseLong (nth toks 2))]
    (if (= h 1) (println "0")
      (let [m (long (Math/ceil (Math/sqrt p)))
            table (loop [j 0 cur 1 m2 {}] (if (>= j m) m2 (recur (inc j) (mod (* cur g) p) (if (contains? m2 cur) m2 (assoc m2 cur j)))))
            gm (modpow g m p)
            [_ ix _] (egcd gm p)
            factor (if (= 1 (first (egcd gm p))) (mod ix p) (modpow g (- p 1 m) p))]
        (loop [i 0 gamma h]
          (cond (>= i m) (println "NONE")
                (contains? table gamma) (let [ans (+ (* i m) (get table gamma))] (if (= h (modpow g ans p)) (println ans) (recur (inc i) (mod (* gamma factor) p))))
                :else (recur (inc i) (mod (* gamma factor) p))))))))
(-main)
