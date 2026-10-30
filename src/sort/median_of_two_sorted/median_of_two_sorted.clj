(ns median-of-two-sorted)
(defn find-median [A B]
  (let [[A B] (if (<= (count A) (count B)) [A B] [B A]) m (count A) n (count B) total (+ m n)]
    (if (and (= m 0) (= n 0)) 0.0
        (loop [lo 0 hi m]
          (if (> lo hi) 0.0
              (let [cutA (quot (+ lo hi) 2) cutB (- (quot (inc total) 2) cutA)
                    maxLA (if (= cutA 0) Long/MIN_VALUE (nth A (dec cutA)))
                    minRA (if (= cutA m) Long/MAX_VALUE (nth A cutA))
                    maxLB (if (= cutB 0) Long/MIN_VALUE (nth B (dec cutB)))
                    minRB (if (= cutB n) Long/MAX_VALUE (nth B cutB))]
                (if (and (<= maxLA minRB) (<= maxLB minRA))
                  (if (odd? total) (double (max maxLA maxLB)) (/ (+ (max maxLA maxLB) (min minRA minRB)) 2.0))
                  (if (> maxLA minRB) (recur lo (dec cutA)) (recur (inc cutA) hi)))))))))
(defn -main [& _]
  (let [[m n] (map #(Long/parseLong %) (clojure.string/split (clojure.string/trim (read-line)) #"\s+"))
        vals (vec (mapcat #(map (fn [x] (Long/parseLong x)) (filter seq (clojure.string/split (clojure.string/trim (or (read-line) "")) #"\s+"))) [(read-line) (read-line)]))
        A (sort (subvec vals 0 (min m (count vals)))) B (sort (subvec vals (min m (count vals)) (count vals)))
        ans (find-median (vec A) (vec B))]
    (if (= ans (long ans)) (println (long ans)) (println ans))))
