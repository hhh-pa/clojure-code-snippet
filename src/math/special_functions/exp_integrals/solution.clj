;; Exponential integrals (NR 6.3).
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

(def euler 0.577215664901533)
(def eps 2.220446049250313e-16)
(def big (* 1.7976931348623157e308 eps))
(def fpmin 1.0020841800044864e-292)

(defn expint [n x]
  (let [nm1 (dec n)]
    (cond (zero? n) (/ (Math/exp (- x)) x)
          (zero? x) (/ 1.0 nm1)
          (> x 1.0) (loop [i 1 b (+ x n) c big d (/ 1.0 (+ x n)) h (/ 1.0 (+ x n))]
                      (if (> i 100) (* h (Math/exp (- x)))
                          (let [a (* (- i) (+ nm1 i))
                                b (+ b 2.0)
                                d (/ 1.0 (+ (* a d) b))
                                c (+ b (/ a c))
                                del (* c d)
                                h (* h del)]
                            (if (<= (absv (- del 1.0)) eps)
                              (* h (Math/exp (- x)))
                              (recur (inc i) b c d h)))))
          :else (loop [i 1 fact 1.0 ans (if (zero? nm1) (- (- (Math/log x)) euler) (/ 1.0 nm1))]
                  (if (> i 100) ans
                      (let [fact (* fact (/ (- x) i))
                            del (if (= i nm1)
                                  (let [psi (loop [ii 1 s (- euler)]
                                              (if (> ii nm1) s (recur (inc ii) (+ s (/ 1.0 ii)))))]
                                    (* fact (+ (- (Math/log x)) psi)))
                                  (/ (- fact) (- i nm1)))
                            ans (+ ans del)]
                        (if (< (absv del) (* (absv ans) eps)) ans (recur (inc i) fact ans))))))))

(defn eifunc [x]
  (cond (< x fpmin) (+ (Math/log x) euler)
        (<= x (- (Math/log eps)))
        (loop [k 1 fact 1.0 sum 0.0]
          (if (> k 100) (+ sum (Math/log x) euler)
              (let [fact (* fact (/ x k))
                    term (/ fact k)
                    sum (+ sum term)]
                (if (< term (* eps sum)) (+ sum (Math/log x) euler) (recur (inc k) fact sum)))))
        :else (loop [k 1 term 1.0 sum 0.0]
                (if (> k 100) (/ (* (Math/exp x) (+ 1.0 sum)) x)
                    (let [prev term
                          term (* term (/ k x))]
                      (cond (< term eps) (/ (* (Math/exp x) (+ 1.0 sum)) x)
                            (< term prev) (recur (inc k) term (+ sum term))
                            :else (/ (* (Math/exp x) (+ 1.0 (- sum prev))) x)))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          ans (if (= op "EN")
                (expint (Long/parseLong (nth toks 1)) (Double/parseDouble (nth toks 2)))
                (eifunc (Double/parseDouble (nth toks 1))))]
      (println (fmt6 (canon ans))))))
