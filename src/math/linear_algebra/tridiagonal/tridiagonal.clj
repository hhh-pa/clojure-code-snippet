(ns tridiagonal
  (:require [clojure.string :as str]))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))
        n (when (seq tokens) (try (Integer/parseInt (first tokens)) (catch Exception _ nil)))]
    (when (and n (pos? n))
      (let [vals (mapv #(Double/parseDouble %) (rest tokens))]
        (if (= n 1)
          (let [d (nth vals 0 0.0) r (nth vals 1 0.0)]
            (if (< (Math/abs d) 1e-12) (println "SINGULAR")
              (println (format "%.6f" (/ r d)))))
          (let [lo (subvec vals 0 (dec n))
                diag (subvec vals (dec n) (+ (dec n) n))
                hi (subvec vals (+ (dec n) n) (+ (dec n) n (dec n)))
                rhs (subvec vals (+ (dec n) n (dec n)) (+ (dec n) n (dec n) n))
                cp (vec (repeat (dec n) 0.0))
                dp (vec (repeat n 0.0))
                valid (atom true)
                cp (atom cp) dp (atom dp)]
            (if (< (Math/abs (nth diag 0)) 1e-12) (reset! valid false)
              (do (swap! cp assoc 0 (/ (nth hi 0) (nth diag 0)))
                  (swap! dp assoc 0 (/ (nth rhs 0) (nth diag 0)))
                  (doseq [i (range 1 (dec n))]
                    (when @valid
                      (let [denom (- (nth diag i) (* (nth lo (dec i)) (nth @cp (dec i))))]
                        (if (< (Math/abs denom) 1e-12) (reset! valid false)
                          (do (swap! cp assoc i (/ (nth hi i) denom))
                              (swap! dp assoc i (/ (- (nth rhs i) (* (nth lo (dec i)) (nth @dp (dec i)))) denom)))))))
                  (when @valid
                    (let [denom (- (nth diag (dec n)) (* (nth lo (- n 2)) (nth @cp (- n 2))))]
                      (if (< (Math/abs denom) 1e-12) (reset! valid false)
                        (swap! dp assoc (dec n) (/ (- (nth rhs (dec n)) (* (nth lo (- n 2)) (nth @dp (- n 2)))) denom)))))))
            (if (not @valid) (println "SINGULAR")
              (let [x (atom (assoc (vec (repeat n 0.0)) (dec n) (nth @dp (dec n))))]
                (doseq [i (range (- n 2) -1 -1)]
                  (swap! x assoc i (- (nth @dp i) (* (nth @cp i) (nth @x (inc i))))))
                (println (str/join " " (map #(format "%.6f" (double %)) @x)))))))))))

(-main)
