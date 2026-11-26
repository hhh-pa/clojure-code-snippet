;; Nonsymmetric eigensystem preprocessing toolkit: Osborne balancing
;; (balance/balbak) and reduction to upper Hessenberg form by the
;; elimination method (elmhes) with accumulated transformation (eltran).
;; Reference: Press et al., Numerical Recipes 3e, Section 11.6.
;; The QR iteration for Hessenberg matrices (11.7: hqr/hqr2/sort/sortvecs)
;; is Webnote-only (Webnote 17), not in the book, so eigenvalues and
;; eigenvectors of general nonsymmetric matrices are not computed here.
;; Documented decisions: RADIX is hardcoded to 2.0 (the book derives
;; numeric_limits<Doub>::radix, "2 for all modern machines"); perm[0] and
;; perm[n-1] are unset by the book (only 1..n-2 defined), so we
;; emit identity there for determinism (eltran never reads them).
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
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

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn dmat [n m] (vec (repeatedly n #(double-array m))))

(defn balance [a ^doubles scale]
  (let [n (count a)
        radix 2.0
        sqrdx (* radix radix)]
    (loop [done false]
      (when (not done)
        (let [done2 (loop [i 0 done true]
                      (if (>= i n) done
                        (let [[r c] (loop [j 0 r 0.0 c 0.0]
                                      (if (>= j n) [r c]
                                        (if (= j i)
                                          (recur (inc j) r c)
                                          (recur (inc j)
                                                 (+ r (absv (dget a i j)))
                                                 (+ c (absv (dget a j i)))))))]
                          (if (and (not= c 0.0) (not= r 0.0))
                            (let [g (/ r radix)
                                  s (+ c r)
                                  [f c] (loop [f 1.0 c c]
                                          (if (< c g)
                                            (recur (* f radix) (* c sqrdx))
                                            [f c]))
                                  g (* r radix)
                                  [f c] (loop [f f c c]
                                          (if (> c g)
                                            (recur (/ f radix) (/ c sqrdx))
                                            [f c]))]
                              (if (< (/ (+ c r) f) (* 0.95 s))
                                (do
                                  (aset ^doubles scale i (* (aget ^doubles scale i) f))
                                  (let [g (/ 1.0 f)]
                                    (dotimes [j n]
                                      (dset! a i j (* (dget a i j) g)))
                                    (dotimes [j n]
                                      (dset! a j i (* (dget a j i) f))))
                                  (recur (inc i) false))
                                (recur (inc i) done)))
                            (recur (inc i) done)))))]
          (recur done2))))))

(defn balbak [zz ^doubles scale]
  (let [n (count zz)]
    (dotimes [i n]
      (dotimes [j n]
        (dset! zz i j (* (dget zz i j) (aget ^doubles scale i)))))))

(defn elmhes [a ^ints perm]
  (let [n (count a)]
    (loop [m 1]
      (when (< m (dec n))
        (let [[x i] (loop [j m x 0.0 i m]
                      (if (>= j n) [x i]
                        (if (> (absv (dget a j (dec m))) (absv x))
                          (recur (inc j) (dget a j (dec m)) j)
                          (recur (inc j) x i))))]
          (aset ^ints perm m (int i))
          (when (not= i m)
            (loop [j (dec m)]
              (when (< j n)
                (let [t (dget a i j)]
                  (dset! a i j (dget a m j))
                  (dset! a m j t))
                (recur (inc j))))
            (loop [j 0]
              (when (< j n)
                (let [t (dget a j i)]
                  (dset! a j i (dget a j m))
                  (dset! a j m t))
                (recur (inc j)))))
          (when (not= x 0.0)
            (loop [i (inc m)]
              (when (< i n)
                (let [y (dget a i (dec m))]
                  (when (not= y 0.0)
                    (let [y (/ y x)]
                      (dset! a i (dec m) y)
                      (loop [j m]
                        (when (< j n)
                          (dset! a i j (- (dget a i j) (* y (dget a m j))))
                          (recur (inc j))))
                      (loop [j 0]
                        (when (< j n)
                          (dset! a j m (+ (dget a j m) (* y (dget a j i))))
                          (recur (inc j)))))))
                (recur (inc i))))))
        (recur (inc m))))))

(defn eltran [a ^ints perm zz]
  (let [n (count a)]
    (dotimes [i n]
      (dotimes [j n]
        (dset! zz i j (if (= i j) 1.0 0.0))))
    (loop [mp (- n 2)]
      (when (> mp 0)
        (loop [k (inc mp)]
          (when (< k n)
            (dset! zz k mp (dget a k (dec mp)))
            (recur (inc k))))
        (let [i (int (aget ^ints perm mp))]
          (when (not= i mp)
            (loop [j mp]
              (when (< j n)
                (dset! zz mp j (dget zz i j))
                (dset! zz i j 0.0)
                (recur (inc j))))
            (dset! zz i mp 1.0)))
        (recur (dec mp))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))]
      (cond
        (= op "BALANCE")
        (let [vals (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (* n n)))))
              a (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                           (range n)))
              scale (double-array (repeat n 1.0))]
          (balance a scale)
          (println (str/join " " (concat (mapcat (fn [i] (map #(fmt6 (canon (dget a i %)))
                                                              (range n)))
                                                   (range n))
                                          (map #(fmt6 (canon (aget ^doubles scale %)))
                                               (range n))))))
        (= op "HESSENBERG")
        (let [vals (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (* n n)))))
              a (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                           (range n)))
              perm (int-array (range n))]
          (elmhes a perm)
          (println (str/join " " (concat (mapcat (fn [i] (map #(fmt6 (canon (dget a i %)))
                                                              (range n)))
                                                   (range n))
                                          (map #(str (aget ^ints perm %)) (range n))))))
        (= op "ELTRAN")
        (let [vals (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (* n n)))))
              a (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                           (range n)))
              perm (int-array (map #(Long/parseLong %) (subvec toks (+ 2 (* n n))
                                                                  (+ 2 (* n n) n))))
              zz (dmat n n)]
          (eltran a perm zz)
          (println (str/join " " (mapcat (fn [i] (map #(fmt6 (canon (dget zz i %)))
                                                       (range n)))
                                            (range n)))))
        (= op "BALBAK")
        (let [vals (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (* n n)))))
              zz (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                            (range n)))
              scale (double-array (map #(Double/parseDouble %)
                                        (subvec toks (+ 2 (* n n)) (+ 2 (* n n) n))))]
          (balbak zz scale)
          (println (str/join " " (mapcat (fn [i] (map #(fmt6 (canon (dget zz i %)))
                                                       (range n)))
                                            (range n)))))))))
