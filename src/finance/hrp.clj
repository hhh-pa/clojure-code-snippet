;; hrp: Hierarchical Risk Parity (AFML ch.16). Single-linkage clustering
;; on correlation distance (first-min lex order), quasi-diagonal leaf
;; order, recursive bisection with inverse-variance cluster weights.
;; Batch 6 (T373). Input: n, covariance matrix.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn cvar [S items iv]
  (let [ws (reduce + (map #(nth iv %) items))]
    (reduce + (mapcat (fn [i] (map (fn [j] (* (/ (nth iv i) ws) (/ (nth iv j) ws) (nth (nth S i) j))) items)) items))))
(defn expand [L R root n]
  (loop [tmp [root] out []]
    (if (empty? tmp) out
      (let [u (peek tmp) rest (pop tmp)]
        (if (< u n) (recur rest (conj out u)) (recur (conj (conj rest (nth R u)) (nth L u)) out))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (Long/parseLong (nth toks 0))
            S (mapv (fn [r] (mapv (fn [c] (Double/parseDouble (nth toks (+ 1 (+ (* r n) c))))) (range n))) (range n))
            sd (mapv (fn [i] (Math/sqrt (nth (nth S i) i))) (range n))
            D (mapv (fn [i] (mapv (fn [j]
                                    (let [rho (max -1.0 (min 1.0 (/ (nth (nth S i) j) (* (nth sd i) (nth sd j)))))]
                                      (Math/sqrt (max 0.0 (* 0.5 (- 1.0 rho)))))) (range n))) (range n))
            N2 (dec (* 2 n))
            res (loop [m 0 alive (vec (concat (repeat n true) (repeat (dec n) false)))
                       CD (mapv (fn [i] (mapv (fn [j] (if (and (< i n) (< j n)) (nth (nth D i) j) 1e100)) (range N2))) (range N2))
                       L (vec (repeat N2 -1)) R (vec (repeat N2 -1)) next n]
                  (if (>= m (dec n)) [L R]
                    (let [best (loop [i 0 b 1e100 bi 0 bj 0]
                                 (if (>= i next) [b bi bj]
                                   (if-not (nth alive i) (recur (inc i) b bi bj)
                                     (let [[b2 bi2 bj2] (loop [j (inc i) b2 b bi2 bi bj2 bj]
                                                          (if (>= j next) [b2 bi2 bj2]
                                                            (if-not (nth alive j) (recur (inc j) b2 bi2 bj2)
                                                              (if (< (nth (nth CD i) j) b2)
                                                                (recur (inc j) (nth (nth CD i) j) i j)
                                                                (recur (inc j) b2 bi2 bj2)))))]
                                       (recur (inc i) b2 bi2 bj2)))))
                          [_ bi bj] best
                          alive2 (assoc (assoc (assoc alive bi false) bj false) next true)
                          CD2 (reduce (fn [a i]
                                        (if-not (nth alive2 i) a
                                          (let [dd (min (nth (nth CD bi) i) (nth (nth CD bj) i))]
                                            (-> a (assoc-in [next i] dd) (assoc-in [i next] dd))))) CD (range next))]
                      (recur (inc m) alive2 CD2 (assoc L next bi) (assoc R next bj) (inc next)))))
            [L R] res
            order (expand L R (dec N2) n)
            iv (mapv (fn [i] (/ 1.0 (nth (nth S i) i))) (range n))
            w (loop [work [order] w (vec (repeat n 1.0))]
                (if (empty? work) w
                  (let [cur (peek work) rest (pop work)]
                    (if (< (count cur) 2) (recur rest w)
                      (let [mid (quot (count cur) 2)
                            Lc (subvec cur 0 mid) Rc (subvec cur mid)
                            vl (cvar S Lc iv) vr (cvar S Rc iv)
                            a (- 1.0 (/ vl (+ vl vr)))
                            w2 (reduce (fn [x i] (assoc x i (* (nth x i) a))) w Lc)
                            w3 (reduce (fn [x i] (assoc x i (* (nth x i) (- 1.0 a)))) w2 Rc)]
                        (recur (conj (conj rest Lc) Rc) w3))))))
            ws (reduce + w)]
        (println (str "ORDER:" (apply str (map (fn [x] (str " " x)) order))))
        (println (str "WEIGHTS:" (apply str (map (fn [x] (str " " (fmt "%.6f" (/ x ws)))) w))))))))
(-main (slurp *in*))
