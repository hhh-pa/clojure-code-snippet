;; Quadratic and cubic equations (NR 5.6).
(require '[clojure.string :as str])

(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        ip (long x)
        fp (long (+ (* (- x ip) 1000000.0) 0.5))
        [ip fp] (if (= fp 1000000) [(inc ip) 0] [ip fp])]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

(defn absv [v] (if (neg? v) (- v) v))

(defn canon [v] (if (< (absv v) 5e-7) 0.0 v))

(defn fmt2 [r im]
  (str (fmt6 (canon r)) " " (fmt6 (canon im))))

(defn quad [a b c]
  (let [disc (- (* b b) (* 4.0 a c))]
    (if (>= disc 0.0)
      (let [sq (Math/sqrt disc)
            q (* -0.5 (+ b (if (neg? b) (- sq) sq)))]
        (if (zero? q) [[0.0 0.0] [0.0 0.0]] [[(/ q a) 0.0] [(/ c q) 0.0]]))
      (let [sr 0.0
            si (Math/sqrt (- disc))
            [sr si] (if (neg? (* b sr)) [(- sr) (- si)] [sr si])
            qr (* -0.5 (+ b sr))
            qi (* -0.5 si)]
        (if (and (zero? qr) (zero? qi)) [[0.0 0.0] [0.0 0.0]]
            (let [den (+ (* qr qr) (* qi qi))]
              [[(/ qr a) (/ qi a)] [(/ (* c qr) den) (/ (- (* c qi)) den)]]))))))

(defn cubic [a b c]
  (let [q (/ (- (* a a) (* 3.0 b)) 9.0)
        r (/ (+ (- (* 2.0 a a a) (* 9.0 a b)) (* 27.0 c)) 54.0)]
    (if (< (* r r) (* q q q))
      (let [th (Math/acos (max -1.0 (min 1.0 (/ r (Math/sqrt (* q q q))))))
            s (* -2.0 (Math/sqrt q))]
        [[(- (* s (Math/cos (/ th 3.0))) (/ a 3.0)) 0.0]
         [(- (* s (Math/cos (/ (+ th (* 2.0 Math/PI)) 3.0))) (/ a 3.0)) 0.0]
         [(- (* s (Math/cos (/ (- th (* 2.0 Math/PI)) 3.0))) (/ a 3.0)) 0.0]])
      (let [sq (Math/sqrt (max 0.0 (- (* r r) (* q q q))))
            av (* (- (if (neg? r) -1.0 1.0)) (Math/pow (+ (absv r) sq) (/ 1.0 3.0)))
            bv (if (zero? av) 0.0 (/ q av))
            x1 (- (+ av bv) (/ a 3.0))
            re (- (* -0.5 (+ av bv)) (/ a 3.0))
            im (* (/ (Math/sqrt 3.0) 2.0) (- av bv))]
        [[x1 0.0] [re im] [re (- im)]]))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond (= op "QUAD") (doseq [[r im] (quad (Double/parseDouble (nth toks 1)) (Double/parseDouble (nth toks 2)) (Double/parseDouble (nth toks 3)))]
                              (println (fmt2 r im)))
            (= op "CUBIC") (doseq [[r im] (cubic (Double/parseDouble (nth toks 1)) (Double/parseDouble (nth toks 2)) (Double/parseDouble (nth toks 3)))]
                               (println (fmt2 r im)))
            (= op "ASINH") (let [x (Double/parseDouble (nth toks 1))
                                 v (if (>= x 0.0) (Math/log (+ x (Math/sqrt (+ (* x x) 1.0))))
                                       (- (Math/log (+ (- x) (Math/sqrt (+ (* x x) 1.0))))))]
                             (println (fmt6 (canon v))))
            :else (let [x (Double/parseDouble (nth toks 1))
                        v (Math/log (+ x (Math/sqrt (- (* x x) 1.0))))]
                    (println (fmt6 (canon v))))))))
