(ns computational-geometry.voronoi.fortune-voronoi
  (:require [clojure.string :as str]))

(def ^:const EPS 1e-9)

(defn circumcircle [[^double ax ^double ay] [^double bx ^double by] [^double cx ^double cy]]
  (let [d (* 2.0 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))]
    (when (> (Math/abs d) EPS)
      (let [ux (/ (+ (* (+ (* ax ax) (* ay ay)) (- by cy))
                     (* (+ (* bx bx) (* by by)) (- cy ay))
                     (* (+ (* cx cx) (* cy cy)) (- ay by)))
                  d)
            uy (/ (+ (* (+ (* ax ax) (* ay ay)) (- cx bx))
                     (* (+ (* bx bx) (* by by)) (- ax cx))
                     (* (+ (* cx cx) (* cy cy)) (- bx ax)))
                  d)
            r-sq (+ (* (- ax ux) (- ax ux)) (* (- ay uy) (- ay uy)))]
        [[ux uy] r-sq]))))

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
                coord-tokens (take (* 2 n) (rest tokens))
                pts (vec (for [[x y] (partition 2 coord-tokens)]
                           [(Double/parseDouble x) (Double/parseDouble y)]))
                raw-centers
                (vec (for [i (range n)
                           j (range (inc i) n)
                           k (range (inc j) n)
                           :let [circ (circumcircle (nth pts i) (nth pts j) (nth pts k))]
                           :when circ
                           :let [[[ux uy] r-sq] circ
                                 valid? (every? (fn [m]
                                                  (if (or (= m i) (= m j) (= m k))
                                                    true
                                                    (let [[^double mx ^double my] (nth pts m)
                                                          d-sq (+ (* (- mx ux) (- mx ux))
                                                                  (* (- my uy) (- my uy)))]
                                                      (>= d-sq (- r-sq EPS)))))
                                                (range n))]
                           :when valid?]
                       [ux uy]))
                sorted-centers (sort point-cmp raw-centers)
                unique-centers (reduce (fn [acc pt]
                                         (if (and (seq acc) (zero? (point-cmp (peek acc) pt)))
                                           acc
                                           (conj acc pt)))
                                       []
                                       sorted-centers)]
            (println (count unique-centers))
            (doseq [[x y] unique-centers]
              (println (String/format java.util.Locale/US "%.2f %.2f" (to-array [(double x) (double y)]))))))))))

(-main)
