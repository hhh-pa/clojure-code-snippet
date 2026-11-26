;; Clenshaw's recurrence (NR 5.4).
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
          f0 (Double/parseDouble (nth toks 1))
          f1 (Double/parseDouble (nth toks 2))
          c (vec (for [i (range (inc n))] (Double/parseDouble (nth toks (+ 3 i)))))
          al (vec (for [i (range (inc n))] (if (zero? i) 0.0 (Double/parseDouble (nth toks (+ 3 (inc n) (dec i)))))))
          be (vec (for [i (range (+ n 2))] (if (zero? i) 0.0 (Double/parseDouble (nth toks (+ 3 (inc n) n (dec i)))))))
          y (loop [k n y (vec (repeat (+ n 3) 0.0))]
              (if (< k 1) y
                  (recur (dec k) (assoc y k (+ (* (nth al k) (nth y (inc k)))
                                                  (* (nth be (inc k)) (nth y (+ k 2)))
                                                  (nth c k))))))
          f (+ (* (nth be 1) f0 (nth y 2)) (* f1 (nth y 1)) (* f0 (nth c 0)))]
      (println (fmt6 (canon f))))))
