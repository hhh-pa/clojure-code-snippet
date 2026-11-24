;; Spherical harmonics (NR 6.7).
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

(defn plegendre [l m x]
  (let [pmm (if (zero? m) 1.0
                (loop [i 1 pmm 1.0 fact 1.0]
                  (if (> i m) pmm
                      (let [omx2 (* (- 1.0 x) (+ 1.0 x))]
                        (recur (inc i) (* pmm (/ (* omx2 fact) (+ fact 1.0))) (+ fact 2.0))))))
        pmm (* (Math/sqrt (/ (* (+ (* 2.0 m) 1.0) pmm) (* 4.0 Math/PI))) (if (odd? m) -1.0 1.0))]
    (cond (= l m) pmm
          (= l (+ m 1)) (* x (Math/sqrt (+ (* 2.0 m) 3.0)) pmm)
          :else (loop [ll (+ m 2) pmm pmm pmmp1 (* x (Math/sqrt (+ (* 2.0 m) 3.0)) pmm)
                       oldfact (Math/sqrt (+ (* 2.0 m) 3.0))]
                  (if (> ll l) pmmp1
                      (let [fact (Math/sqrt (/ (- (* 4.0 ll ll) 1.0) (- (* ll ll) (* m m))))
                            pll (* (- (* x pmmp1) (/ pmm oldfact)) fact)]
                        (recur (inc ll) pmmp1 pll fact)))))))

(defn plgndr [l m x]
  (let [prod (loop [j (+ (- l m) 1) p 1.0]
               (if (> j (+ l m)) p (recur (inc j) (* p j))))]
    (* (Math/sqrt (/ (* 4.0 Math/PI prod) (+ (* 2.0 l) 1.0))) (plegendre l m x))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (if (or (= op "PLEG") (= op "PLM"))
        (let [l (Long/parseLong (nth toks 1))
              m (Long/parseLong (nth toks 2))
              x (Double/parseDouble (nth toks 3))
              ans (if (= op "PLEG") (plegendre l m x) (plgndr l m x))]
          (println (fmt6 (canon ans))))
        (let [l (Long/parseLong (nth toks 1))
              m (Long/parseLong (nth toks 2))
              th (Double/parseDouble (nth toks 3))
              ph (Double/parseDouble (nth toks 4))
              [re im] (if (>= m 0)
                        (let [pt (plegendre l m (Math/cos th))]
                          [(* pt (Math/cos (* m ph))) (* pt (Math/sin (* m ph)))])
                        (let [pt (plegendre l (- m) (Math/cos th))
                              s (if (odd? (- m)) -1.0 1.0)]
                          [(* s pt (Math/cos (* m ph))) (* s pt (Math/sin (* m ph)))]))]
          (println (fmt6 (canon re)) (fmt6 (canon im))))))))
