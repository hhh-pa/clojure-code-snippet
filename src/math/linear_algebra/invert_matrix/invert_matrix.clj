(ns invert-matrix
  (:require [clojure.string :as str]))

(def MOD 1000000007)
(defn mod-norm [x] (let [r (mod x MOD)] (if (< r 0) (+ r MOD) r)))
(defn mod-pow [b e] (loop [base (mod-norm b) exp e acc 1] (if (<= exp 0) acc (recur (mod (* base base) MOD) (quot exp 2) (if (odd? exp) (mod (* acc base) MOD) acc)))))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))
        idx (atom 0)
        nxt (fn [] (let [v (nth tokens @idx)] (swap! idx inc) v))
        n (when (seq tokens) (try (Integer/parseInt (nxt)) (catch Exception _ nil)))]
    (when (and n (pos? n))
      (let [a (vec (for [i (range n)] (vec (for [j (range n)] (mod-norm (Long/parseLong (nxt)))))))
            aug (vec (for [i (range n)] (vec (concat (nth a i) (assoc (vec (repeat n 0)) i 1)))))
            result (loop [col 0 aug aug]
                     (if (>= col n) {:aug aug :singular false}
                       (let [pivot (first (filter #(not= 0 (get-in aug [% col])) (range col n)))]
                         (if (nil? pivot) {:singular true}
                           (let [aug (if (= pivot col) aug (let [t (nth aug col)] (-> aug (assoc col (nth aug pivot)) (assoc pivot t))))
                                 inv (mod-pow (get-in aug [col col]) (- MOD 2))
                                 aug (assoc aug col (mapv #(mod (* % inv) MOD) (nth aug col)))
                                 aug (reduce (fn [aug row]
                                               (if (or (= row col) (= 0 (get-in aug [row col]))) aug
                                                 (let [f (get-in aug [row col])]
                                                   (assoc aug row (mapv (fn [j] (mod-norm (- (nth (nth aug row) j) (mod (* f (nth (nth aug col) j)) MOD)))) (range (* 2 n)))))))
                                             aug (range n))]
                             (recur (inc col) aug))))))]
        (if (:singular result)
          (println "SINGULAR")
          (doseq [i (range n)]
            (println (str/join " " (subvec (nth (:aug result) i) n)))))))))

(-main)
