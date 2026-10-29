(ns computational-geometry.sweep-line.bentley-ottmann
  (:require [clojure.string :as str]))

(def ^:const EPS 1e-9)

(defn intersect [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [x1 (double x1) y1 (double y1)
        x2 (double x2) y2 (double y2)
        x3 (double x3) y3 (double y3)
        x4 (double x4) y4 (double y4)
        denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (> (Math/abs denom) EPS)
      (let [t (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom)
            u (/ (- (* (- x1 x2) (- y1 y3)) (* (- y1 y2) (- x1 x3))) (- denom))]
        (when (and (>= t (- EPS)) (<= t (+ 1.0 EPS))
                   (>= u (- EPS)) (<= u (+ 1.0 EPS)))
          [(+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1)))])))))

(defn point-cmp [[^double x1 ^double y1] [^double x2 ^double y2]]
  (if (> (Math/abs (- x1 x2)) EPS)
    (compare x1 x2)
    (if (> (Math/abs (- y1 y2)) EPS)
      (compare y1 y2)
      0)))

(defn -main [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (remove str/blank? (str/split content #"\s+"))]
        (when (seq tokens)
          (let [n (Long/parseLong (first tokens))
                seg-tokens (take (* 4 n) (rest tokens))
                segs (vec (for [[x1 y1 x2 y2] (partition 4 seg-tokens)]
                            (let [p1 [(Double/parseDouble x1) (Double/parseDouble y1)]
                                  p2 [(Double/parseDouble x2) (Double/parseDouble y2)]]
                              (if (pos? (point-cmp p1 p2))
                                [p2 p1]
                                [p1 p2]))))
                raw-pts (vec (for [i (range n)
                                   j (range (inc i) n)
                                   :let [pt (intersect (nth segs i) (nth segs j))]
                                   :when pt]
                               pt))
                sorted-pts (sort point-cmp raw-pts)
                unique-pts (reduce (fn [acc pt]
                                     (if (and (seq acc) (zero? (point-cmp (peek acc) pt)))
                                       acc
                                       (conj acc pt)))
                                   []
                                   sorted-pts)]
            (println (count unique-pts))
            (doseq [[x y] unique-pts]
              (println (String/format java.util.Locale/US "%.2f %.2f" (to-array [(double x) (double y)]))))))))))

(-main)
