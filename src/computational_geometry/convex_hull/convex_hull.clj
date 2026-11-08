(require '[clojure.string :as str])

;; Convex hull by monotone chain with perimeter calculation.
;; Time: O(n log n), Space: O(n).
;; Mirrors python-code-snippet/src/computational_geometry/convex_hull/convex_hull.py.

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- dist [[x1 y1] [x2 y2]]
  (Math/hypot (double (- x1 x2)) (double (- y1 y2))))

(defn- cross [[ox oy] [ax ay] [bx by]]
  (- (* (- ax ox) (- by oy))
     (* (- ay oy) (- bx ox))))

(defn- hull [points]
  (let [ans (transient [])]
    (doseq [p points]
      (loop []
        (let [n (count ans)]
          (when (>= n 2)
            (let [o (nth ans (- n 2))
                  a (nth ans (dec n))]
              (when (<= (cross o a p) 0)
                (pop! ans)
                (recur))))))
      (conj! ans p))
    (when (pos? (count ans))
      (pop! ans))
    (persistent! ans)))

(defn- convex-hull [points]
  (let [distinct-points (vec (sort (set points)))]
    (if (> (count distinct-points) 2)
      (let [lower (hull distinct-points)
            upper (hull (rseq distinct-points))]
        (into lower upper))
      distinct-points)))

(defn- perimeter [p]
  (let [n (count p)]
    (if (zero? n)
      0.0
      (loop [i 0 ans 0.0]
        (if (= i (dec n))
          (+ ans (dist (nth p 0) (nth p (dec n))))
          (recur (inc i) (+ ans (dist (nth p i) (nth p (inc i))))))))))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [n (parse-long (nth toks 0))
                  points (loop [i 0 idx 1 acc (transient [])]
                           (if (= i n)
                             (persistent! acc)
                             (let [x (parse-long (nth toks idx))
                                   y (parse-long (nth toks (inc idx)))]
                               (recur (inc i) (+ idx 2) (conj! acc [x y])))))
                  ans (perimeter (convex-hull points))]
              (println (format "%.1f" ans)))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
