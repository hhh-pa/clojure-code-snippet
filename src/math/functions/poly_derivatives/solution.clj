;; Polynomial and derivatives at a point (NR 5.1, ddpoly).
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
          m (Long/parseLong (nth toks 1))
          c (vec (for [i (range (inc n))] (Double/parseDouble (nth toks (+ 2 i)))))
          x (Double/parseDouble (nth toks (+ 2 (inc n))))
          pd (loop [i (dec n) pd (assoc (vec (repeat (inc m) 0.0)) 0 (nth c n))]
               (if (< i 0) pd
                   (let [nnd (min m (- n i))
                         pd (loop [j nnd pd pd]
                              (if (< j 1) pd
                                  (recur (dec j) (assoc pd j (+ (* (nth pd j) x) (nth pd (dec j)))))))]
                     (recur (dec i) (assoc pd 0 (+ (* (nth pd 0) x) (nth c i)))))))
          pd (loop [i 2 cnst 1.0 pd pd]
               (if (> i m) pd (recur (inc i) (* cnst i) (assoc pd i (* (nth pd i) (* cnst i))))))]
      (println (str/join " " (map #(fmt6 (canon %)) pd))))))
