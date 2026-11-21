;; Singular value decomposition (NR 2.6 contract): one-sided Jacobi.
;; Port of the C++ reference.
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

(defn rot-cols [mat m p q c s]
  (reduce (fn [mt k]
            (let [vp (get-in mt [k p]) vq (get-in mt [k q])]
              (assoc-in (assoc-in mt [k p] (- (* c vp) (* s vq)))
                        [k q] (+ (* s vp) (* c vq)))))
          mat (range m)))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          m (Long/parseLong (nth toks 1))
          n (Long/parseLong (nth toks 2))
          nums (mapv #(Double/parseDouble %) (subvec toks 3))
          a (vec (map #(vec (subvec nums (* % n) (+ (* % n) n))) (range m)))
          [b thresh] (cond (= op "SOLVE")
                           [(vec (subvec nums (* m n) (+ (* m n) m)))
                            (nth nums (+ (* m n) m))]
                           (or (= op "RANK") (= op "SUBSPACE"))
                           [nil (nth nums (* m n))]
                           :else [nil 0.0])
          v0 (vec (map #(vec (map (fn [j] (if (= % j) 1.0 0.0)) (range n))) (range n)))
          [bb v] (reduce (fn [[bb v] _]
                            (reduce (fn [[bb v] [p q]]
                                      (let [[alpha gamma beta]
                                            (reduce (fn [[al ga be] k]
                                                      [(+ al (* (get-in bb [k p]) (get-in bb [k p])))
                                                       (+ ga (* (get-in bb [k q]) (get-in bb [k q])))
                                                       (+ be (* (get-in bb [k p]) (get-in bb [k q])))])
                                                    [0.0 0.0 0.0] (range m))]
                                        (if (zero? beta)
                                          [bb v]
                                          (let [zeta (/ (- gamma alpha) (* 2.0 beta))
                                                t (/ (if (neg? zeta) -1.0 1.0)
                                                     (+ (absv zeta) (Math/sqrt (+ 1.0 (* zeta zeta)))))
                                                c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                                                s (* c t)]
                                            [(rot-cols bb m p q c s)
                                             (rot-cols v n p q c s)]))))
                                    [bb v]
                                    (for [p (range (dec n)) q (range (inc p) n)] [p q])))
                          [a v0] (range 12))
          w (mapv (fn [j] (Math/sqrt (reduce + (map #(let [x (get-in bb [% j])] (* x x)) (range m))))) (range n))
          u (vec (for [i (range m)]
                   (mapv (fn [j] (if (zero? (nth w j)) 0.0 (/ (get-in bb [i j]) (nth w j)))) (range n))))
          perm (vec (sort-by #(nth w %) > (range n)))
          w (mapv #(nth w %) perm)
          u (vec (map (fn [i] (mapv #(nth (nth u i) %) perm)) (range m)))
          v (vec (map (fn [i] (mapv #(nth (nth v i) %) perm)) (range n)))]
      (cond
        (= op "FACTOR")
        (print (str "U\n"
                    (str/join "\n" (map #(str/join " " (map (fn [x] (fmt6 (canon x))) %)) u))
                    "\nW\n" (str/join " " (map #(fmt6 (canon %)) w))
                    "\nV\n" (str/join "\n" (map #(str/join " " (map (fn [x] (fmt6 (canon x))) %)) v))))
        (= op "SOLVE")
        (let [tmp (mapv (fn [j]
                          (if (> (nth w j) thresh)
                            (/ (reduce + (map #(* (get-in u [% j]) (nth b %)) (range m))) (nth w j))
                            0.0))
                        (range n))
              x (mapv (fn [j] (reduce + (map #(* (get-in v [j %]) (nth tmp %)) (range n)))) (range n))]
          (print (str/join " " (map #(fmt6 (canon %)) x))))
        (= op "RANK")
        (let [rank (count (filter #(> (nth w %) thresh) (range n)))]
          (print (str rank " " (- n rank))))
        (= op "SUBSPACE")
        (let [rg (vec (filter #(> (nth w %) thresh) (range n)))
              ns (vec (filter #(<= (nth w %) thresh) (range n)))
              urow (fn [j] (str/join " " (map (fn [i] (fmt6 (canon (get-in u [i j])))) (range m))))
              vrow (fn [j] (str/join " " (map (fn [i] (fmt6 (canon (get-in v [i j])))) (range n))))]
          (print (str "RANGE " (count rg)
                      (str/join "" (map #(str "\n" (urow %)) rg))
                      "\nNULLSPACE " (count ns)
                      (str/join "" (map #(str "\n" (vrow %)) ns)))))
        :else
        (print (fmt6 (canon (if (or (<= (nth w 0) 0.0) (<= (nth w (dec n)) 0.0))
                                0.0 (/ (nth w (dec n)) (nth w 0))))))))))
