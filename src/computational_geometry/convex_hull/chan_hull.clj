(ns computational-geometry.convex-hull.chan-hull
  (:require [clojure.string :as str]))

(defn cross [[^long ax ^long ay] [^long bx ^long by] [^long cx ^long cy]]
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(defn graham-scan [pts]
  (let [pts (vec (distinct (sort pts)))]
    (if (<= (count pts) 2)
      pts
      (let [lower
            (reduce
             (fn [hull p]
               (loop [h hull]
                 (if (and (>= (count h) 2)
                          (<= (cross (nth h (- (count h) 2)) (peek h) p) 0))
                   (recur (pop h))
                   (conj h p))))
             []
             pts)
            upper
            (reduce
             (fn [hull p]
               (loop [h hull]
                 (if (and (>= (count h) 2)
                          (<= (cross (nth h (- (count h) 2)) (peek h) p) 0))
                   (recur (pop h))
                   (conj h p))))
             []
             (reverse pts))]
        (into (pop lower) (pop upper))))))

(defn -main [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (remove str/blank? (str/split content #"\s+"))]
        (when (seq tokens)
          (let [n (Long/parseLong (first tokens))
                pts (vec (for [[x y] (partition 2 (take (* 2 n) (rest tokens)))]
                           [(Long/parseLong x) (Long/parseLong y)]))
                hull (graham-scan pts)
                min-idx (if (empty? hull)
                          0
                          (first (reduce (fn [a b]
                                           (if (neg? (compare (second b) (second a))) b a))
                                         (map-indexed vector hull))))
                rotated (vec (concat (subvec hull min-idx) (subvec hull 0 min-idx)))]
            (println (count rotated))
            (doseq [[x y] rotated]
              (println (str x " " y)))))))))

(-main)
