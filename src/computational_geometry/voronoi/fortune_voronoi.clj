(require '[clojure.string :as str])

(def ^:const eps 1e-9)

(defn circumcircle [[ax ay] [bx by] [cx cy]]
  (let [d (* 2.0 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))]
    (when (>= (abs d) eps)
      (let [a2 (+ (* ax ax) (* ay ay))
            b2 (+ (* bx bx) (* by by))
            c2 (+ (* cx cx) (* cy cy))
            ux (/ (+ (* a2 (- by cy)) (* b2 (- cy ay)) (* c2 (- ay by))) d)
            uy (/ (+ (* a2 (- cx bx)) (* b2 (- ax cx)) (* c2 (- bx ax))) d)
            dx (- ax ux)
            dy (- ay uy)
            r-sq (+ (* dx dx) (* dy dy))]
        [[ux uy] r-sq]))))

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
                pts (loop [i 0 idx 1 acc (transient [])]
                      (if (= i n)
                        (persistent! acc)
                        (let [x (parse-double (nth toks idx))
                              y (parse-double (nth toks (inc idx)))]
                          (recur (inc i) (+ idx 2) (conj! acc [x y])))))
                centers (loop [i 0 acc (transient [])]
                          (if (= i n)
                            (persistent! acc)
                            (let [pi (nth pts i)
                                  nacc (loop [j (inc i) acc acc]
                                         (if (= j n)
                                           acc
                                           (let [pj (nth pts j)
                                                 nacc2 (loop [k (inc j) acc acc]
                                                         (if (= k n)
                                                           acc
                                                           (if-let [[center r-sq] (circumcircle pi pj (nth pts k))]
                                                             (let [cx (nth center 0)
                                                                   cy (nth center 1)
                                                                   valid? (loop [m 0]
                                                                            (if (= m n)
                                                                              true
                                                                              (if (or (= m i) (= m j) (= m k))
                                                                                (recur (inc m))
                                                                                (let [pm (nth pts m)
                                                                                      dx (- (nth pm 0) cx)
                                                                                      dy (- (nth pm 1) cy)
                                                                                      dist-sq (+ (* dx dx) (* dy dy))]
                                                                                  (if (< dist-sq (- r-sq eps))
                                                                                    false
                                                                                    (recur (inc m)))))))]
                                                               (if valid?
                                                                 (recur (inc k) (conj! acc center))
                                                                 (recur (inc k) acc)))
                                                             (recur (inc k) acc))))]
                                             (recur (inc j) nacc2))))]
                              (recur (inc i) nacc))))
                sorted-centers (vec (sort centers))
                m (count sorted-centers)
                unique-centers (loop [i 0 acc (transient []) prev nil]
                                 (if (= i m)
                                   (persistent! acc)
                                   (let [c (nth sorted-centers i)]
                                     (if (or (nil? prev)
                                             (> (abs (- (nth c 0) (nth prev 0))) eps)
                                             (> (abs (- (nth c 1) (nth prev 1))) eps))
                                       (recur (inc i) (conj! acc c) c)
                                       (recur (inc i) acc prev)))))]
            (println (count unique-centers))
            (doseq [c (seq unique-centers)]
              (println (format "%.2f %.2f" (nth c 0) (nth c 1))))))))))

(-main)
