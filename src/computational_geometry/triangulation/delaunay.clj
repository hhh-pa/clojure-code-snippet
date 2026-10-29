(ns computational-geometry.triangulation.delaunay
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

(defn -main [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (remove str/blank? (str/split content #"\s+"))]
        (when (seq tokens)
          (let [n (Long/parseLong (first tokens))
                coord-tokens (take (* 2 n) (rest tokens))
                pts (vec (for [[x y] (partition 2 coord-tokens)]
                           [(Double/parseDouble x) (Double/parseDouble y)]))
                delaunay-tris
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
                       [i j k]))
                sorted-tris (sort-by (fn [[i j k]] [i j k]) delaunay-tris)]
            (println (count sorted-tris))
            (doseq [[i j k] sorted-tris]
              (println (str i " " j " " k)))))))))

(-main)
