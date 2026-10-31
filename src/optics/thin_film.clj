;; thin_film: thin-film multilayer characteristic matrix method — for s and p
;; polarizations the amplitude reflection/transmission and R/T of a layer
;; stack on a substrate. Batch 6 (T390).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn sn [x] (if (< (Math/abs (double x)) 5e-7) 0.0 x))
(defn cmul [a b] [(- (* (nth a 0) (nth b 0)) (* (nth a 1) (nth b 1)))
                  (+ (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))])
(defn cadd [a b] [(+ (nth a 0) (nth b 0)) (+ (nth a 1) (nth b 1))])
(defn csub [a b] [(- (nth a 0) (nth b 0)) (- (nth a 1) (nth b 1))])
(defn cdiv [a b] (let [d (+ (* (nth b 0) (nth b 0)) (* (nth b 1) (nth b 1)))]
                   [(/ (+ (* (nth a 0) (nth b 0)) (* (nth a 1) (nth b 1))) d)
                    (/ (- (* (nth a 1) (nth b 0)) (* (nth a 0) (nth b 1))) d)]))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [PI Math/PI
            L (Long/parseLong (nth toks 0))
            lam (Double/parseDouble (nth toks 1)) th0 (Double/parseDouble (nth toks 2))
            n0 (Double/parseDouble (nth toks 3)) ns (Double/parseDouble (nth toks 4))
            nn (mapv (fn [j] (Double/parseDouble (nth toks (+ 5 (* j 2))))) (range L))
            dd (mapv (fn [j] (Double/parseDouble (nth toks (+ 6 (* j 2))))) (range L))
            s0 (* n0 (Math/sin (* th0 PI (/ 1.0 180.0))))]
        (loop [pol 0]
          (when (< pol 2)
            (let [[M11 M12 M21 M22]
                  (loop [j 0 M11 [1.0 0.0] M12 [0.0 0.0] M21 [0.0 0.0] M22 [1.0 0.0]]
                    (if (>= j L) [M11 M12 M21 M22]
                      (let [st0 (/ s0 (nth nn j))
                            st (cond (> st0 1) 1.0 (< st0 -1) -1.0 :else st0)
                            ct (Math/sqrt (max 0.0 (- 1 (* st st))))
                            eta (if (= pol 0) (* (nth nn j) ct) (/ (nth nn j) ct))
                            delta (/ (* 2 PI (nth nn j) (nth dd j) ct) lam)
                            c [(Math/cos delta) 0.0] sv [(Math/sin delta) 0.0] iu [0.0 1.0]
                            N11 c N12 (cdiv (cmul iu sv) [eta 0.0])
                            N21 (cmul (cmul iu [eta 0.0]) sv) N22 c]
                        (recur (inc j)
                               (cadd (cmul M11 N11) (cmul M12 N21))
                               (cadd (cmul M11 N12) (cmul M12 N22))
                               (cadd (cmul M21 N11) (cmul M22 N21))
                               (cadd (cmul M21 N12) (cmul M22 N22))))))
                  ct0 (Math/cos (* th0 PI (/ 1.0 180.0)))
                  sts0 (/ s0 ns)
                  sts (cond (> sts0 1) 1.0 (< sts0 -1) -1.0 :else sts0)
                  cts (Math/sqrt (max 0.0 (- 1 (* sts sts))))
                  e0 (if (= pol 0) (* n0 ct0) (/ n0 ct0))
                  es (if (= pol 0) (* ns cts) (/ ns cts))
                  e0c [e0 0.0] esc [es 0.0]
                  den (cadd (cadd (cadd (cmul e0c M11) (cmul [(* e0 es) 0.0] M12)) M21) (cmul esc M22))
                  r (cdiv (csub (csub (cadd (cmul e0c M11) (cmul [(* e0 es) 0.0] M12)) M21) (cmul esc M22)) den)
                  t (cdiv [(* 2.0 e0) 0.0] den)
                  R (+ (* (nth r 0) (nth r 0)) (* (nth r 1) (nth r 1)))
                  T (* (/ es e0) (+ (* (nth t 0) (nth t 0)) (* (nth t 1) (nth t 1))))]
              (println (fmt "%s: r %.6f %.6f R %.6f T %.6f" (if (= pol 0) "S" "P")
                              (sn (+ (nth r 0) 0.0)) (sn (+ (nth r 1) 0.0)) (sn (+ R 0.0)) (sn (+ T 0.0)))))
            (recur (inc pol))))))))
(-main (slurp *in*))
