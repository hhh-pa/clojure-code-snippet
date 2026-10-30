(ns solution
  (:require [clojure.string :as str]))

(defn has-common [strs L]
  (if (zero? L) ""
    (let [s0 (first strs)
          common (set (for [i (range (inc (- (count s0) L)))] (subs s0 i (+ i L))))]
      (loop [remaining (rest strs) cur common]
        (if (empty? remaining) (first cur)
          (let [s (first remaining)
                nxt (set (filter cur (for [i (range (inc (- (count s) L)))] (subs s i (+ i L)))))]
            (if (empty? nxt) nil
              (recur (rest remaining) nxt))))))))

(defn lcs-k [strs]
  (let [k (count strs)]
    (cond
      (zero? k) [0 ""]
      (= k 1) [(count (first strs)) (first strs)]
      :else
      (let [hi (apply min (map count strs))
            best (loop [lo 0 hi hi bl 0 bs ""]
                   (if (> lo hi) [bl bs]
                     (let [mid (quot (+ lo hi) 2)
                           cur (has-common strs mid)]
                       (if (some? cur)
                         (recur (inc mid) hi mid cur)
                         (recur lo (dec mid) bl bs)))))]
        (if (zero? (first best)) best
          (let [L (first best)
                s0 (first strs)
                common (set (for [i (range (inc (- (count s0) L)))] (subs s0 i (+ i L))))
                final-common (reduce (fn [cur s]
                                       (set (filter cur (for [i (range (inc (- (count s) L)))] (subs s i (+ i L))))))
                                     common (rest strs))]
            [L (first (sort final-common))]))))))

(defn -main []
  (let [input (slurp *in*)
        toks (filter seq (str/split input #"\s+"))
        k (if (seq toks) (Integer/parseInt (first toks)) 0)
        strs (vec (take k (rest toks)))
        [best s] (lcs-k strs)]
    (println best)
    (println s)))

(-main)
