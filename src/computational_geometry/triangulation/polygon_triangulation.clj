(ns computational-geometry.triangulation.polygon-triangulation
  (:require [clojure.string :as str]))

(def ^:const EPS 1e-9)

(defn cross [a b c]
  (let [ax (double (:x a)) ay (double (:y a))
        bx (double (:x b)) by (double (:y b))
        cx (double (:x c)) cy (double (:y c))]
    (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))))

(defn point-in-triangle [p a b c]
  (let [cp1 (cross a b p)
        cp2 (cross b c p)
        cp3 (cross c a p)
        has-neg (or (< cp1 (- EPS)) (< cp2 (- EPS)) (< cp3 (- EPS)))
        has-pos (or (> cp1 EPS) (> cp2 EPS) (> cp3 EPS))]
    (not (and has-neg has-pos))))

(defn -main [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (remove str/blank? (str/split content #"\s+"))]
        (when (seq tokens)
          (let [n (Long/parseLong (first tokens))
                coord-tokens (take (* 2 n) (rest tokens))
                poly (vec (map-indexed (fn [i [x y]]
                                         {:x (Double/parseDouble x)
                                          :y (Double/parseDouble y)
                                          :id i})
                                       (partition 2 coord-tokens)))
                signed-area (reduce + (for [i (range n)
                                            :let [next-i (rem (inc i) n)]]
                                        (- (* (:x (nth poly i)) (:y (nth poly next-i)))
                                           (* (:x (nth poly next-i)) (:y (nth poly i))))))
                poly (if (< signed-area 0.0) (vec (reverse poly)) poly)]

            (loop [cur-poly poly
                   triangles []]
              (if (<= (count cur-poly) 3)
                (let [final-tris (if (= (count cur-poly) 3)
                                   (conj triangles (vec (sort (map :id cur-poly))))
                                   triangles)
                      sorted-tris (sort-by (fn [[i j k]] [i j k]) final-tris)]
                  (println (count sorted-tris))
                  (doseq [[i j k] sorted-tris]
                    (println (str i " " j " " k))))

                (let [sz (count cur-poly)
                      ear (loop [i 0]
                            (if (>= i sz)
                              nil
                              (let [prev-i (rem (+ i sz -1) sz)
                                    next-i (rem (inc i) sz)
                                    a (nth cur-poly prev-i)
                                    b (nth cur-poly i)
                                    c (nth cur-poly next-i)]
                                (if (<= (cross a b c) EPS)
                                  (recur (inc i))
                                  (let [is-ear? (every? (fn [j]
                                                          (if (or (= j prev-i) (= j i) (= j next-i))
                                                            true
                                                            (not (point-in-triangle (nth cur-poly j) a b c))))
                                                        (range sz))]
                                    (if is-ear?
                                      [i (vec (sort [(:id a) (:id b) (:id c)]))]
                                      (recur (inc i))))))))]
                  (if-not ear
                    ;; break if no ear found
                    (let [sorted-tris (sort-by (fn [[i j k]] [i j k]) triangles)]
                      (println (count sorted-tris))
                      (doseq [[i j k] sorted-tris]
                        (println (str i " " j " " k))))
                    (let [[ear-idx tri] ear
                          next-poly (vec (concat (subvec cur-poly 0 ear-idx)
                                                 (subvec cur-poly (inc ear-idx))))]
                      (recur next-poly (conj triangles tri)))))))))))))

(-main)
