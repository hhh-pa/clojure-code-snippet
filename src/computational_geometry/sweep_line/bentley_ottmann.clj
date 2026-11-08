(require '[clojure.string :as str])

(def ^:const eps 1e-9)

(defn intersect [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (>= (abs denom) eps)
      (let [t (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom)
            u (/ (- (* (- x1 x2) (- y1 y3)) (* (- y1 y2) (- x1 x3))) (- denom))]
        (when (and (>= t (- eps)) (<= t (+ 1.0 eps))
                   (>= u (- eps)) (<= u (+ 1.0 eps)))
          [(+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1)))])))))

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn -main []
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [toks (tokenize content)
            nt (count toks)]
        (when (pos? nt)
          (let [n (parse-long (nth toks 0))
                segs (loop [i 0 idx 1 acc (transient [])]
                       (if (= i n)
                         (persistent! acc)
                         (let [x1 (parse-double (nth toks idx))
                               y1 (parse-double (nth toks (+ idx 1)))
                               x2 (parse-double (nth toks (+ idx 2)))
                               y2 (parse-double (nth toks (+ idx 3)))
                               p1 [x1 y1]
                               p2 [x2 y2]
                               [p1 p2] (if (or (< x2 (- x1 eps))
                                               (and (<= (abs (- x2 x1)) eps)
                                                    (< y2 (- y1 eps))))
                                         [p2 p1]
                                         [p1 p2])]
                           (recur (inc i) (+ idx 4) (conj! acc [p1 p2])))))
                pts (loop [i 0 acc (transient [])]
                      (if (= i n)
                        (persistent! acc)
                        (let [s1 (nth segs i)
                              nacc (loop [j (inc i) acc acc]
                                     (if (= j n)
                                       acc
                                       (if-let [pt (intersect s1 (nth segs j))]
                                         (recur (inc j) (conj! acc pt))
                                         (recur (inc j) acc))))]
                          (recur (inc i) nacc))))
                sorted-pts (vec (sort pts))
                m (count sorted-pts)
                unique-pts (loop [i 0 acc (transient []) prev nil]
                             (if (= i m)
                               (persistent! acc)
                               (let [pt (nth sorted-pts i)]
                                 (if (or (nil? prev)
                                         (> (abs (- (nth pt 0) (nth prev 0))) eps)
                                         (> (abs (- (nth pt 1) (nth prev 1))) eps))
                                   (recur (inc i) (conj! acc pt) pt)
                                   (recur (inc i) acc prev)))))]
            (println (count unique-pts))
            (doseq [pt (seq unique-pts)]
              (println (format "%.2f %.2f" (nth pt 0) (nth pt 1))))))))))

(-main)
