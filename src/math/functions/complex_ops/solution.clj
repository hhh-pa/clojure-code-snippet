;; Complex arithmetic (NR 5.5).
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

(defn cdiv [ar ai br bi]
  (if (>= (absv br) (absv bi))
    (let [t (/ bi br) den (+ br (* bi t))]
      [(/ (+ ar (* ai t)) den) (/ (- ai (* ar t)) den)])
    (let [t (/ br bi) den (+ bi (* br t))]
      [(/ (+ (* ar t) ai) den) (/ (- (* ai t) ar) den)])))

(defn csqrt [ar ai]
  (let [w (if (and (zero? ar) (zero? ai)) 0.0
              (if (>= (absv ar) (absv ai))
                (let [t (/ ai ar)]
                  (* (Math/sqrt (absv ar)) (Math/sqrt (/ (+ 1.0 (Math/sqrt (+ 1.0 (* t t)))) 2.0))))
                (let [t (/ ar ai)]
                  (* (Math/sqrt (absv ai)) (Math/sqrt (/ (+ (absv t) (Math/sqrt (+ 1.0 (* t t)))) 2.0))))))]
    (cond (zero? w) [0.0 0.0]
          (>= ar 0.0) [w (/ ai (* 2.0 w))]
          (>= ai 0.0) [(/ (absv ai) (* 2.0 w)) w]
          :else [(/ (absv ai) (* 2.0 w)) (- w)])))

(defn cexp [ar ai]
  (let [e (Math/exp ar)] [(* e (Math/cos ai)) (* e (Math/sin ai))]))

(defn clog [ar ai]
  [(Math/log (Math/hypot ar ai)) (Math/atan2 ai ar)])

(defn cpow [ar ai br bi]
  (let [[lr li] (clog ar ai)
        er (- (* lr br) (* li bi))
        ei (+ (* lr bi) (* li br))
        e (Math/exp er)]
    [(* e (Math/cos ei)) (* e (Math/sin ei))]))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          ar (Double/parseDouble (nth toks 1))
          ai (Double/parseDouble (nth toks 2))
          [r im] (cond (= op "DIV") (cdiv ar ai (Double/parseDouble (nth toks 3)) (Double/parseDouble (nth toks 4)))
                       (= op "SQRT") (csqrt ar ai)
                       (= op "EXP") (cexp ar ai)
                       (= op "LOG") (clog ar ai)
                       :else (cpow ar ai (Double/parseDouble (nth toks 3)) (Double/parseDouble (nth toks 4))))]
      (println (fmt6 (canon r)) (fmt6 (canon im))))))
