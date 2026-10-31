(ns simultaneous-min-max)
(defn simultaneous-min-max [a]
  (let [n (count a)]
    (cond (= n 0) [0 0] (= n 1) [(first a) (first a)]
          :else (let [even? (even? n)
                      [mn mx] (if even? (if (< (a 0) (a 1)) [(a 0) (a 1)] [(a 1) (a 0)]) [(a 0) (a 0)])
                      start (if even? 2 1)]
                  (loop [i start cur-mn mn cur-mx mx]
                    (if (>= (inc i) n) [cur-mn cur-mx]
                        (let [x (a i) y (a (inc i)) sm (min x y) lg (max x y)]
                          (recur (+ i 2) (min cur-mn sm) (max cur-mx lg)))))))))
(defn -main [& _]
  (let [n (Long/parseLong (clojure.string/trim (read-line)))
        line (clojure.string/trim (or (read-line) ""))]
    (if (= n 0) (println "0 0")
        (let [a (vec (map #(Long/parseLong %) (filter seq (clojure.string/split line #"\s+"))))
              [mn mx] (simultaneous-min-max a)]
          (println mn mx)))))

(-main)
