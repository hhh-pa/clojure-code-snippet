(ns lu-decomposition
  (:require [clojure.string :as str]))

(def MOD 1000000007)

(defn mod-norm [x] (let [r (mod x MOD)] (if (< r 0) (+ r MOD) r)))
(defn mod-pow [b e] (loop [base (mod-norm b) exp e acc 1] (if (<= exp 0) acc (recur (mod (* base base) MOD) (quot exp 2) (if (odd? exp) (mod (* acc base) MOD) acc)))))

(defn solve-lu [a pi b]
  (let [n (count a)
        y (loop [i 0 y (vec (repeat n 0))]
            (if (>= i n) y
              (let [s (reduce (fn [s j] (mod-norm (- s (mod (* (get-in a [i j]) (nth y j)) MOD)))) (nth b (nth pi i)) (range i))]
                (recur (inc i) (assoc y i s)))))
        x (loop [i (dec n) x (vec (repeat n 0))]
            (if (< i 0) x
              (let [s (reduce (fn [s j] (mod-norm (- s (mod (* (get-in a [i j]) (nth x j)) MOD)))) (nth y i) (range (inc i) n))]
                (recur (dec i) (assoc x i (mod (* s (mod-pow (get-in a [i i]) (- MOD 2))) MOD))))))]
    x))

(defn -main []
  (let [tokens (str/split (str/trim (slurp *in*)) #"\s+")
        idx (atom 0)
        nxt (fn [] (let [v (nth tokens @idx)] (swap! idx inc) v))
        n (when (seq tokens) (try (Integer/parseInt (nxt)) (catch Exception _ nil)))]
    (when (and n (pos? n))
      (let [a (vec (for [i (range n)] (vec (for [j (range n)] (mod-norm (Long/parseLong (nxt)))))))
            b (vec (for [i (range n)] (mod-norm (Long/parseLong (nxt)))))
            pi (vec (range n))
            result (loop [k 0 a a pi pi swaps 0]
                     (if (>= k n)
                       {:a a :pi pi :swaps swaps :singular false}
                       (let [p (apply max 0 (map #(nth (nth a %) k) (range k n)))
                             kp (first (filter #(= (nth (nth a %) k) p) (range k n)))
                             kp (or kp k)]
                         (if (zero? p) {:singular true}
                           (let [pi2 (if (= kp k) pi (let [t (nth pi k)] (-> pi (assoc k (nth pi kp)) (assoc kp t))))
                                 a2 (if (= kp k) a (let [t (nth a k)] (-> a (assoc k (nth a kp)) (assoc kp t))))
                                 inv (mod-pow (get-in a2 [k k]) (- MOD 2))
                                 a3 (reduce (fn [a i]
                                              (let [a (assoc-in a [i k] (mod (* (get-in a [i k]) inv) MOD))]
                                                (reduce (fn [a j] (assoc-in a [i j] (mod-norm (- (get-in a [i j]) (mod (* (get-in a [i k]) (get-in a [k j])) MOD))))) a (range (inc k) n))))
                                            a2 (range (inc k) n))]
                             (recur (inc k) a3 pi2 (if (= kp k) swaps (inc swaps))))))))]
        (if (:singular result)
          (println "SINGULAR")
          (let [a (:a result) pi (:pi result) swaps (:swaps result)
                det (let [d (reduce (fn [acc k] (mod (* acc (get-in a [k k])) MOD)) 1 (range n))] (if (and (odd? swaps) (not= d 0)) (- MOD d) d))
                x (solve-lu a pi b)
                inv-mat (for [c (range n)]
                          (let [e (assoc (vec (repeat n 0)) c 1)] (solve-lu a pi e)))]
            (println det)
            (println (str/join " " x))
            (doseq [r (range n)] (println (str/join " " (map #(nth (nth inv-mat % ) r) (range n)))))))))))

(-main)
