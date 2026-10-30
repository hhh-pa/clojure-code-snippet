;; kmeans: Lloyd's k-means clustering with first-k init, fixed iterations.
;; Input: n d k iters then n points. Output: assignments, then centers %.4f.
(ns kmeans (:require [clojure.string :as str]))
(def ^:private US java.util.Locale/US)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Double/parseDouble t))
(defn fmt [x] (String/format US "%.4f" (into-array Object [(+ (double x) 0.0)])))
(defn dist2 [p c]
  (reduce + (map (fn [a b] (let [d (- a b)] (* d d))) p c)))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 4)
      (let [n (long (Double/parseDouble (nth toks 0)))
            d (long (Double/parseDouble (nth toks 1)))
            k (long (Double/parseDouble (nth toks 2)))
            iters (long (Double/parseDouble (nth toks 3)))
            vals (mapv parse-token (drop 4 toks))
            pts (vec (for [i (range n)] (vec (for [j (range d)] (nth vals (+ (* i d) j))))))
            [asg centers]
            (loop [t 0 centers (vec (take k pts)) asg (vec (repeat n 0))]
              (if (>= t iters) [asg centers]
                (let [asg2 (mapv (fn [i]
                                   (loop [cc 0 best -1.0 bi 0]
                                     (if (>= cc k) bi
                                       (let [dd (dist2 (nth pts i) (nth centers cc))]
                                         (if (or (neg? best) (< dd best))
                                           (recur (inc cc) dd cc)
                                           (recur (inc cc) best bi))))))
                                 (range n))
                      c2 (mapv (fn [cc]
                                 (let [mem (filterv #(= cc (nth asg2 %)) (range n))]
                                   (if (empty? mem) (nth centers cc)
                                     (mapv (fn [j] (/ (reduce + (map #(nth (nth pts %) j) mem)) (count mem))) (range d)))))
                               (range k))]
                  (recur (inc t) c2 asg2))))]
        (println (str/join " " (map str asg)))
        (doseq [cc (range k)]
          (println (str/join " " (map fmt (nth centers cc)))))))))
(-main)
