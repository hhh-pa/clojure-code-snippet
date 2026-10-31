(ns bfprt-select)
(defn bfprt [a k]
  (if (= (count a) 1) (first a)
      (let [n (count a) ng (int (Math/ceil (/ n 5.0)))
            medians (for [i (range ng)] (let [l (* i 5) r (min (* (inc i) 5) n) grp (sort (subvec a l r))] (nth grp (quot (count grp) 2))))
            pivot (let [s (sort medians)] (nth s (quot (count s) 2)))
            less (filter #(< % pivot) a) eq (filter #(= % pivot) a) greater (filter #(> % pivot) a)
            rank-lt (count less) rank-eq (count eq)]
        (cond (< k rank-lt) (bfprt (vec less) k)
              (< k (+ rank-lt rank-eq)) pivot
              :else (bfprt (vec greater) (- k rank-lt rank-eq))))))
(defn -main [& _]
  (let [n (Long/parseLong (clojure.string/trim (read-line)))]
    (if (= n 0) (println 0)
        (let [line (clojure.string/trim (or (read-line) ""))
              a (vec (map #(Long/parseLong %) (filter seq (clojure.string/split line #"\s+"))))
              k (dec (Long/parseLong (clojure.string/trim (or (read-line) "0"))))]
          (if (or (< k 0) (>= k n)) (println 0) (println (bfprt a k)))))))

(-main)
