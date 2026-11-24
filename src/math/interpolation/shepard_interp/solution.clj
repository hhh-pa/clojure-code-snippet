;; Shepard interpolation (NR 3.7.4).
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
    (let [p (Long/parseLong (nth toks 0))
          n (Long/parseLong (nth toks 1))
          dim (Long/parseLong (nth toks 2))
          nums (mapv #(Double/parseDouble %) (subvec toks 3))
          pts (vec (for [i (range n)] (vec (subvec nums (* i dim) (+ (* i dim) dim)))))
          vals (vec (subvec nums (* n dim) (+ (* n dim) n)))
          q (long (nth nums (+ (* n dim) n)))
          qs (vec (for [i (range q)]
                    (vec (subvec nums (+ (+ (* n dim) n 1) (* i dim)) (+ (+ (* n dim) n 1) (* (inc i) dim))))))]
      (print (str/join "\n"
                       (map (fn [pt]
                              (let [r (loop [i 0 num 0.0 den 0.0]
                                          (if (>= i n)
                                            (/ num den)
                                            (let [d (Math/sqrt (reduce + 0.0 (map (fn [j] (let [dd (- (nth pt j) (get-in pts [i j]))] (* dd dd))) (range dim))))]
                                              (if (zero? d)
                                                (nth vals i)
                                                (let [rp (loop [k 0 rp 1.0] (if (>= k p) rp (recur (inc k) (* rp d))))
                                                      w (/ 1.0 rp)]
                                                  (recur (inc i) (+ num (* w (nth vals i))) (+ den w)))))))]
                                (fmt6 (canon r))))
                            qs))))))
