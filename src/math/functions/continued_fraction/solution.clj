;; Continued fraction evaluation (NR 5.2, modified Lentz's method).
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
    (let [n (Long/parseLong (nth toks 0))
          b0 (Double/parseDouble (nth toks 1))
          tiny 1.0e-30
          f0 (if (zero? b0) tiny b0)
          res (loop [j 1 f f0 c f0 d 0.0]
                (if (> j n) f
                    (let [a (Double/parseDouble (nth toks (* j 2)))
                          b (Double/parseDouble (nth toks (+ (* j 2) 1)))
                          d1 (if (zero? (+ b (* a d))) tiny (+ b (* a d)))
                          c1 (if (zero? (+ b (/ a c))) tiny (+ b (/ a c)))
                          dinv (/ 1.0 d1)]
                      (recur (inc j) (* f (* c1 dinv)) c1 dinv))))]
      (println (fmt6 (canon res))))))
