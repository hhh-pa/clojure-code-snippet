;; Vandermonde solve in O(n^2) (NR 2.8.1, vander).
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
          nums (mapv #(Double/parseDouble %) (subvec toks 1))
          x (vec (subvec nums 0 n))
          q (vec (subvec nums n (+ n n)))]
      (if (= n 1)
        (print (fmt6 (canon (nth q 0))))
        (let [c (loop [i 1 c (assoc (vec (repeat n 0.0)) (dec n) (- (nth x 0)))]
                  (if (>= i n) c
                      (let [xx (- (nth x i))
                            c (loop [j (- (dec n) i) c c]
                                (if (>= j (dec n)) c
                                    (recur (inc j) (assoc c j (+ (nth c j) (* xx (nth c (inc j))))))))]
                        (recur (inc i) (assoc c (dec n) (+ (nth c (dec n)) xx))))))
              w (mapv (fn [i]
                        (let [xx (nth x i)
                              [_ s t] (loop [k (dec n) b 1.0 s (nth q (dec n)) t 1.0]
                                           (if (<= k 0) [b s t]
                                               (let [b (+ (nth c k) (* xx b))]
                                                 (recur (dec k) b (+ s (* (nth q (dec k)) b)) (+ (* xx t) b)))))]
                          (/ s t)))
                      (range n))]
          (print (str/join " " (map #(fmt6 (canon %)) w))))))))
