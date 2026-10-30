(ns solution
  (:require [clojure.string :as str]))

(defn build-sa [s]
  (let [n (count s)
        b (vec (map int s))]
    (loop [sa (vec (range n)) rank (vec b) k 1]
      (if (>= k n) sa
        (let [nsa (vec (sort-by (fn [i] [(nth rank i) (if (< (+ i k) n) (nth rank (+ i k)) -1)]) sa))
              tmp (loop [i 1 m {(first nsa) 0} prev (first nsa) cur 0]
                    (if (>= i n) m
                      (let [ci (nth nsa i)
                            pr [(nth rank prev) (if (< (+ prev k) n) (nth rank (+ prev k)) -1)]
                            cr [(nth rank ci) (if (< (+ ci k) n) (nth rank (+ ci k)) -1)]
                            nc (if (= pr cr) cur (inc cur))]
                        (recur (inc i) (assoc m ci nc) ci nc))))]
          (recur nsa (vec (map #(get tmp %) (range n))) (* k 2)))))))

(defn build-lcp [s sa]
  (let [n (count s)
        b (vec (map int s))
        rank (into {} (map-indexed (fn [i v] [v i]) sa))]
    (loop [i 0 h 0 lcp (vec (repeat n 0))]
      (if (>= i n) lcp
        (if (zero? (get rank i))
          (recur (inc i) 0 lcp)
          (let [j (nth sa (dec (get rank i)))
                nh (loop [hh h] (if (and (< (+ i hh) n) (< (+ j hh) n) (= (nth b (+ i hh)) (nth b (+ j hh)))) (recur (inc hh)) hh))]
            (recur (inc i) (if (pos? nh) (dec nh) 0) (assoc lcp (get rank i) nh))))))))

(defn -main []
  (let [input (slurp *in*)
        toks (filter seq (str/split input #"\s+"))
        s (first toks)]
    (when s
      (let [n (count s)]
        (if (zero? n)
          (do (println) (println 0))
          (let [sa (build-sa s)
                lcp (build-lcp s sa)]
            (println (str/join " " sa))
            (println (apply max lcp))))))))

(-main)
