;; Polynomial interpolation by Neville's algorithm (NR 3.2).
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

(defn neville [xa ya x]
  (let [n (count xa)
        c (vec ya)
        d (vec ya)
        dif (absv (- x (nth xa 0)))
        [ns _] (loop [i 0 ns 0 dif dif]
                 (if (>= i n) [ns dif]
                     (let [dift (absv (- x (nth xa i)))]
                       (if (< dift dif)
                         (recur (inc i) i dift)
                         (recur (inc i) ns dif)))))
        [y dy] (loop [m 1 y (nth ya ns) c c d d ns (dec ns) dy 0.0]
                   (if (>= m n) [y dy]
                       (let [[c d] (loop [i 0 c c d d]
                                     (if (>= i (- n m)) [c d]
                                         (let [ho (- (nth xa i) x)
                                               hp (- (nth xa (+ i m)) x)
                                               w (- (nth c (inc i)) (nth d i))
                                               den (/ w (- ho hp))]
                                           (recur (inc i)
                                                  (assoc c i (* ho den))
                                                  (assoc d i (* hp den))))))
                             [dy ns] (if (< (* 2 (inc ns)) (- n m))
                                       [(nth c (inc ns)) ns]
                                       [(nth d ns) (dec ns)])]
                         (recur (inc m) (+ y dy) c d ns dy))))]
    [y dy]))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [n (Long/parseLong (nth toks 0))
          nums (mapv #(Double/parseDouble %) (subvec toks 1))
          xa (vec (subvec nums 0 n))
          ya (vec (subvec nums n (+ n n)))
          q (long (nth nums (+ n n)))
          qs (subvec nums (+ n n 1) (+ (+ n n 1) q))]
      (print (str/join "\n" (map (fn [x] (let [[y dy] (neville xa ya x)]
                                           (str (fmt6 (canon y)) " " (fmt6 (canon dy))))) qs))))))
