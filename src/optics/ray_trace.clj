;; ray_trace: exact 3D ray tracing through centered spherical surfaces (R=0
;; plano) — sphere intersection, vector Snell refraction, optical path
;; length. Batch 6 (T387).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn sn [x] (if (< (Math/abs (double x)) 5e-7) 0.0 x))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [S (Long/parseLong (nth toks 0))
            zv (mapv (fn [j] (Double/parseDouble (nth toks (+ 1 (* j 3))))) (range S))
            RR (mapv (fn [j] (Double/parseDouble (nth toks (+ 2 (* j 3))))) (range S))
            na (mapv (fn [j] (Double/parseDouble (nth toks (+ 3 (* j 3))))) (range S))
            M (Long/parseLong (nth toks (+ 1 (* S 3))))
            base (+ 2 (* S 3))]
        (loop [m 0]
          (when (< m M)
            (let [q (+ base (* m 6))
                  ox (Double/parseDouble (nth toks q)) oy (Double/parseDouble (nth toks (+ q 1)))
                  oz (Double/parseDouble (nth toks (+ q 2))) dx0 (Double/parseDouble (nth toks (+ q 3)))
                  dy0 (Double/parseDouble (nth toks (+ q 4))) dz0 (Double/parseDouble (nth toks (+ q 5)))
                  dl (Math/sqrt (+ (* dx0 dx0) (* dy0 dy0) (* dz0 dz0)))
                  res (loop [j 0 ox ox oy oy oz oz dx (/ dx0 dl) dy (/ dy0 dl) dz (/ dz0 dl)
                             n1 1.0 opl 0.0 hx [] hy [] ok true]
                        (if (or (>= j S) (not ok)) [ox oy oz dx dy dz n1 opl hx hy ok]
                          (let [t (if (= (nth RR j) 0.0)
                                      (if (not= dz 0.0) (/ (- (nth zv j) oz) dz) -1.0)
                                      (let [cz (+ (nth zv j) (nth RR j))
                                            ocx ox ocy oy ocz (- oz cz)
                                            b (+ (* dx ocx) (* dy ocy) (* dz ocz))
                                            c (- (+ (* ocx ocx) (* ocy ocy) (* ocz ocz)) (* (nth RR j) (nth RR j)))
                                            disc (- (* b b) c)]
                                        (if (>= disc 0)
                                          (let [sq (Math/sqrt disc) t1 (- (- b) sq) t2 (+ (- b) sq)]
                                            (cond (> t1 1e-9) t1 (> t2 1e-9) t2 :else -1.0))
                                          -1.0)))]
                            (if (< t 1e-9)
                              (recur (inc j) ox oy oz dx dy dz n1 opl hx hy false)
                              (let [opl2 (+ opl (* n1 t))
                                    px (+ ox (* dx t)) py (+ oy (* dy t)) pz (+ oz (* dz t))
                                    nv (if (= (nth RR j) 0.0) [0.0 0.0 1.0]
                                         (let [ar (Math/abs (nth RR j))
                                               nx (/ px ar) ny (/ py ar) nz (/ (- pz (+ (nth zv j) (nth RR j))) ar)
                                               nl (Math/sqrt (+ (* nx nx) (* ny ny) (* nz nz)))]
                                           [(/ nx nl) (/ ny nl) (/ nz nl)]))
                                    dot (+ (* dx (nth nv 0)) (* dy (nth nv 1)) (* dz (nth nv 2)))
                                    nx (if (> dot 0) (- (nth nv 0)) (nth nv 0))
                                    ny (if (> dot 0) (- (nth nv 1)) (nth nv 1))
                                    nz (if (> dot 0) (- (nth nv 2)) (nth nv 2))
                                    eta (/ n1 (nth na j))
                                    c1 (- (+ (* dx nx) (* dy ny) (* dz nz)))
                                    s2t (* eta eta (- 1 (* c1 c1)))]
                                (if (> s2t 1)
                                  (recur (inc j) ox oy oz dx dy dz n1 opl hx hy false)
                                  (let [c2 (Math/sqrt (max 0.0 (- 1 s2t)))
                                        k (- (* eta c1) c2)
                                        dx2 (+ (* eta dx) (* k nx)) dy2 (+ (* eta dy) (* k ny)) dz2 (+ (* eta dz) (* k nz))]
                                    (recur (inc j) (+ px (* dx2 1e-9)) (+ py (* dy2 1e-9)) (+ pz (* dz2 1e-9))
                                           dx2 dy2 dz2 (nth na j) opl2 (conj hx px) (conj hy py) true))))))))]
              (if (nth res 10)
                (do (println (str (fmt "RAY %d HIT:" m)
                                      (apply str (map (fn [j] (fmt " %.6f %.6f" (sn (+ (nth (nth res 8) j) 0.0)) (sn (+ (nth (nth res 9) j) 0.0)))) (range S)))))
                    (println (fmt "RAY %d DIR: %.6f %.6f %.6f" m (sn (+ (nth res 3) 0.0)) (sn (+ (nth res 4) 0.0)) (sn (+ (nth res 5) 0.0))))
                    (println (fmt "RAY %d OPL: %.6f" m (sn (+ (nth res 7) 0.0)))))
                (println (fmt "RAY %d MISS" m))))
            (recur (inc m))))))))
(-main (slurp *in*))
