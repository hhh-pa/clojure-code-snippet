(ns cholesky
  (:require [clojure.string :as str]))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))
        n (when (seq tokens) (try (Integer/parseInt (first tokens)) (catch Exception _ nil)))]
    (when (and n (pos? n))
      (let [vals (mapv #(Double/parseDouble %) (rest tokens))
            a (vec (for [i (range n)] (vec (for [j (range n)] (nth vals (+ (* i n) j) 0.0)))))
            L (vec (repeat n (vec (repeat n 0.0))))
            result (loop [i 0 L L]
                     (if (>= i n) {:L L :ok true}
                       (let [res (loop [j 0 L L]
                                   (if (> j i) {:L L :fail false}
                                     (let [s (reduce (fn [s k] (- s (* (get-in L [i k]) (get-in L [j k])))) (get-in a [i j]) (range j))]
                                       (if (= i j)
                                         (if (<= s 1e-9) {:fail true}
                                           (recur (inc j) (assoc-in L [i j] (Math/sqrt s))))
                                         (recur (inc j) (assoc-in L [i j] (/ s (get-in L [j j]))))))))]
                         (if (:fail res) {:ok false} (recur (inc i) (:L res))))))]
        (if (not (:ok result))
          (println "NOT_POSITIVE_DEFINITE")
          (doseq [i (range n)]
            (println (str/join " " (map #(String/format java.util.Locale/US "%.6f" (into-array Object [(double %)])) (nth (:L result) i))))))))))

(-main)
