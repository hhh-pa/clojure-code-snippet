(ns least-squares
  (:require [clojure.string :as str]))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))
        n0 (when (>= (count tokens) 2) (try [(Integer/parseInt (nth tokens 0)) (Integer/parseInt (nth tokens 1))] (catch Exception _ nil)))]
    (when n0
      (let [[m n] n0
            vals (mapv #(Double/parseDouble %) (drop 2 tokens))
            idx (atom 0)
            nxt (fn [] (let [v (nth vals @idx)] (swap! idx inc) v))
            A (vec (for [i (range m)] (vec (for [j (range n)] (nxt)))))
            b (vec (for [i (range m)] (nxt)))
            AtA (vec (for [i (range n)] (vec (for [j (range n)] (reduce + (map #(* (get-in A [% i]) (get-in A [% j])) (range m)))))))
            Atb (vec (for [i (range n)] (reduce + (map #(* (get-in A [% i]) (nth b %)) (range m)))))
            aug (vec (for [i (range n)] (conj (nth AtA i) (nth Atb i))))
            gauss (loop [col 0 aug aug]
                    (if (>= col n) {:aug aug :ok true}
                      (let [pivot (apply max-key #(Math/abs (double (get-in aug [% col]))) (range col n))
                            best (Math/abs (double (get-in aug [pivot col])))]
                        (if (< best 1e-12) {:ok false}
                          (let [aug (if (= pivot col) aug (let [t (nth aug col)] (-> aug (assoc col (nth aug pivot)) (assoc pivot t))))
                                aug (reduce (fn [aug row]
                                              (let [f (/ (get-in aug [row col]) (get-in aug [col col]))]
                                                (assoc aug row (mapv - (nth aug row) (map #(* f %) (nth aug col))))))
                                            aug (range (inc col) n))]
                            (recur (inc col) aug))))))
            ]
        (if (not (:ok gauss))
          (println "SINGULAR")
          (let [aug (:aug gauss)
                x (loop [i (dec n) x (vec (repeat n 0.0))]
                    (if (< i 0) x
                      (let [s (reduce (fn [s j] (- s (* (get-in aug [i j]) (nth x j)))) (nth (nth aug i) n) (range (inc i) n))]
                        (recur (dec i) (assoc x i (/ s (get-in aug [i i])))))))]
            (println (str/join " " (map #(format "%.6f" (double %)) x)))))))))

(-main)
