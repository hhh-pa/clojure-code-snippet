;; Dawson's integral (NR 6.9).
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

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [x (Double/parseDouble (nth toks 0))
          hstep 0.4
          c (vec (for [i (range 6)]
                   (let [t (* (+ (* 2.0 i) 1.0) hstep)] (Math/exp (- (* t t))))))
          ans (if (< (absv x) 0.2)
                (let [x2 (* x x)]
                  (* x (- 1.0 (* (/ 2.0 3.0) x2 (- 1.0 (* 0.4 x2 (- 1.0 (* (/ 2.0 7.0) x2))))))))
                (let [xx (absv x)
                      n0 (* 2 (long (+ (/ (* 0.5 xx) hstep) 0.5)))
                      xp (- xx (* n0 hstep))
                      e1 (Math/exp (* 2.0 xp hstep))
                      e2 (* e1 e1)]
                  (loop [i 0 d1 (+ n0 1.0) d2 (- (+ n0 1.0) 2.0) e1 e1 sum 0.0]
                    (if (>= i 6)
                      (* 0.5641895835 (if (>= x 0.0) (Math/exp (- (* xp xp))) (- (Math/exp (- (* xp xp))))) sum)
                      (recur (inc i) (+ d1 2.0) (- d2 2.0) (* e1 e2)
                             (+ sum (* (nth c i) (+ (/ e1 d1) (/ 1.0 (* d2 e1))))))))))]
      (println (fmt6 (canon ans))))))
