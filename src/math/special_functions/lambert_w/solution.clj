;; Inverse of y = x log(x), smaller root (NR 6.11).
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

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [y (Double/parseDouble (nth toks 0))
          ooe 0.367879441171442322]
      (if (not (and (> y (- ooe)) (< y 0.0)))
        (println "0.000000")
        (let [u0 (if (< y -0.2)
                   (Math/log (- ooe (Math/sqrt (* (* 2.0 ooe) (+ y ooe)))))
                   -10.0)]
          (loop [u u0 to 0.0]
            (let [t (* (- (Math/log (/ y u)) u) (/ u (+ 1.0 u)))
                  nu (+ u t)]
              (if (and (< t 1.0e-8) (< (absv (+ t to)) (* 0.01 (absv t))))
                (println (fmt6 (canon (Math/exp nu))))
                (if (> (absv (/ t nu)) 1.0e-15)
                  (recur nu t)
                  (println (fmt6 (canon (Math/exp nu)))))))))))))
