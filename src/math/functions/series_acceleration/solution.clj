;; Series acceleration (NR 5.3): epsilon algorithm and Levin u-transform. Port of the C++ reference.
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

(def small 2.2250738585072014e-307)
(def big 1.7976931348623157e308)

(defn epsalg [a n]
  (loop [k 0 nn 0 sum 0.0 e (vec (repeat n 0.0)) val 0.0]
    (if (>= k n) val
        (let [sum (+ sum (nth a k))
              e (assoc e nn sum)
              [e _] (loop [j nn e e temp2 0.0]
                      (if (< j 1) [e temp2]
                          (let [temp1 temp2
                                temp2 (nth e (dec j))
                                diff (- (nth e j) temp2)]
                            (recur (dec j)
                                   (assoc e (dec j)
                                          (if (<= (absv diff) small) big (+ temp1 (/ 1.0 diff))))
                                   temp2))))
              nn (inc nn)]
          (recur (inc k) nn sum e (if (odd? nn) (nth e 0) (nth e 1)))))))

(defn levin [a n]
  (loop [k 0 nn 0 sum 0.0 numer (vec (repeat n 0.0)) denom (vec (repeat n 0.0)) lastval 0.0]
    (if (>= k n) lastval
        (let [sum (+ sum (nth a k))
              omega (* (+ 1.0 nn) (nth a k))
              term0 (/ 1.0 (+ 1.0 nn))
              numer (assoc numer nn (* sum (/ term0 omega)))
              denom (assoc denom nn (/ term0 omega))
              [numer denom] (if (zero? nn) [numer denom]
                                (let [ratio (* (+ 1.0 nn -1.0) term0)]
                                  (loop [j 1 numer numer denom denom term term0]
                                    (if (> j nn) [numer denom]
                                        (let [fact (* (+ (- nn j) 1.0) term)]
                                          (recur (inc j)
                                                 (assoc numer (- nn j)
                                                        (- (nth numer (+ (- nn j) 1))
                                                           (* fact (nth numer (- nn j)))))
                                                 (assoc denom (- nn j)
                                                        (- (nth denom (+ (- nn j) 1))
                                                           (* fact (nth denom (- nn j)))))
                                                 (* term ratio)))))))
              val (if (< (absv (nth denom 0)) small) lastval (/ (nth numer 0) (nth denom 0)))]
          (recur (inc k) (inc nn) sum numer denom val)))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          a (vec (for [i (range n)] (Double/parseDouble (nth toks (+ 2 i)))))
          val (if (= op "EPSALG") (epsalg a n) (levin a n))]
      (println (fmt6 (canon val))))))
