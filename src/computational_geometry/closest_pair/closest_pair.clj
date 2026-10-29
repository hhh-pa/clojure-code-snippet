(ns computational-geometry.closest-pair.closest-pair
  (:require [clojure.string :as str]))

(defn dist [[^double x1 ^double y1] [^double x2 ^double y2]]
  (Math/hypot (- x1 x2) (- y1 y2)))

(defn closest-pair-rec [pts]
  (let [n (count pts)]
    (if (<= n 3)
      (let [min-d (double-array [1e18])]
        (dotimes [i n]
          (dotimes [j (- n (inc i))]
            (let [k (+ (inc i) j)
                  d (dist (nth pts i) (nth pts k))]
              (when (< d (aget min-d 0))
                (aset min-d 0 d)))))
        (aget min-d 0))
      (let [mid (quot n 2)
            mid-x (double (first (nth pts mid)))
            d1 (closest-pair-rec (subvec pts 0 mid))
            d2 (closest-pair-rec (subvec pts mid))
            d (Math/min (double d1) (double d2))
            strip (vec (sort-by (fn [[_ y]] (double y))
                                (filter (fn [[x _]] (< (Math/abs (- (double x) mid-x)) d)) pts)))
            strip-n (count strip)
            d-ref (double-array [d])]
        (dotimes [i strip-n]
          (let [p1 (nth strip i)
                y1 (double (second p1))]
            (loop [j (inc i)]
              (when (< j strip-n)
                (let [p2 (nth strip j)
                      y2 (double (second p2))]
                  (when (< (- y2 y1) (aget d-ref 0))
                    (let [cur-d (dist p1 p2)]
                      (when (< cur-d (aget d-ref 0))
                        (aset d-ref 0 cur-d))
                      (recur (inc j)))))))))
        (aget d-ref 0)))))

(defn -main [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (remove str/blank? (str/split content #"\s+"))]
        (when (seq tokens)
          (let [n (Long/parseLong (first tokens))
                pts (vec (for [[x y] (partition 2 (take (* 2 n) (rest tokens)))]
                           [(Double/parseDouble x) (Double/parseDouble y)]))
                sorted-pts (vec (sort-by (fn [[x y]] [(double x) (double y)]) pts))
                ans (closest-pair-rec sorted-pts)]
            (println (String/format java.util.Locale/US "%.6f" (to-array [ans])))))))))

(-main)
