(ns karatsuba
  (:require [clojure.string :as str]))

(def MOD 1000000007)
(defn mod-norm [x] (let [r (mod x MOD)] (if (< r 0) (+ r MOD) r)))

(defn karatsuba [a b]
  (let [n (count a) m (count b)]
    (cond (or (zero? n) (zero? m)) []
          (<= (min n m) 32)
          (let [c (vec (repeat (+ n m -1) 0))]
            (reduce (fn [c i]
                      (reduce (fn [c j] (update c (+ i j) #(mod (+ % (mod (* (nth a i) (nth b j)) MOD)) MOD)))
                              c (range m)))
                    c (range n)))
          :else
          (let [k (quot (max n m) 2)
                a0 (subvec a 0 (min k n)) a1 (if (> (count a) k) (subvec a k) [])
                b0 (subvec b 0 (min k m)) b1 (if (> (count b) k) (subvec b k) [])
                z0 (karatsuba a0 b0) z2 (karatsuba a1 b1)
                asz (max (count a0) (count a1)) bsz (max (count b0) (count b1))
                asum (vec (for [i (range asz)] (mod (+ (nth a0 i 0) (nth a1 i 0)) MOD)))
                bsum (vec (for [i (range bsz)] (mod (+ (nth b0 i 0) (nth b1 i 0)) MOD)))
                z1 (karatsuba asum bsum)
                z1 (if (< (count z1) (max (count z0) (count z2))) (into z1 (repeat (- (max (count z0) (count z2)) (count z1)) 0)) z1)
                z1 (mapv mod-norm (map - z1 (concat z0 (repeat 0))))
                z1 (mapv mod-norm (map - z1 (concat z2 (repeat 0))))
                res (vec (repeat (+ n m -1) 0))]
            (-> res
                (#(reduce (fn [r i] (update r i (fn [v] (mod (+ v (nth z0 i 0)) MOD)))) % (range (count z0))))
                (#(reduce (fn [r i] (update r (+ i k) (fn [v] (mod (+ v (nth z1 i 0)) MOD)))) % (range (count z1))))
                (#(reduce (fn [r i] (update r (+ i (* 2 k)) (fn [v] (mod (+ v (nth z2 i 0)) MOD)))) % (range (count z2)))))))))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))]
    (when (>= (count tokens) 2)
      (let [da (Integer/parseInt (nth tokens 0))
            db (Integer/parseInt (nth tokens 1))
            a (mapv #(mod-norm (Long/parseLong %)) (subvec tokens 2 (+ 2 (inc da))))
            b (mapv #(mod-norm (Long/parseLong %)) (subvec tokens (+ 2 (inc da)) (+ 2 (inc da) (inc db))))
            c (karatsuba a b) dc (+ da db)]
        (println dc)
        (println (str/join " " c))))))

(-main)
