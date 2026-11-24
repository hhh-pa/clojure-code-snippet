;; All eigenvalues and eigenvectors of a real symmetric
;; matrix by Jacobi's cyclic method (with eigsrt).
;; Reference: Press et al., Numerical Recipes 3e, Section 11.1.
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

(defn eigsrt [^doubles d v]
  (let [n (alength ^doubles d)]
    (loop [i 0]
      (when (< i (dec n))
        (let [[k p] (loop [j i k i p (aget ^doubles d i)]
                        (if (>= j n) [k p]
                          (if (>= (aget ^doubles d j) p)
                            (recur (inc j) j (aget ^doubles d j))
                            (recur (inc j) k p))))]
          (when (not= k i)
            (aset ^doubles d k (aget ^doubles d i))
            (aset ^doubles d i p)
            (dotimes [j n]
              (let [t (dget v j i)]
                (dset! v j i (dget v j k))
                (dset! v j k t)))))
        (recur (inc i))))))

(defn rot [a s tau i j k l]
  (let [g (dget a i j)
        h (dget a k l)]
    (dset! a i j (- g (* s (+ h (* g tau)))))
    (dset! a k l (+ h (* s (- g (* h tau)))))))

(defn jacobi [aa]
  (let [n (count aa)
        eps 2.220446049250313e-16
        a (vec (map (fn [row]
                      (let [r (double-array n)]
                        (dotimes [i n] (aset ^doubles r i (aget ^doubles row i)))
                        r))
                    aa))
        v (dmat n n)
        d (double-array n)
        b (double-array n)
        z (double-array n)]
    (dotimes [ip n]
      (dotimes [iq n] (dset! v ip iq 0.0))
      (dset! v ip ip 1.0))
    (dotimes [ip n]
      (aset ^doubles b ip (dget a ip ip))
      (aset ^doubles d ip (dget a ip ip))
      (aset ^doubles z ip 0.0))
    (loop [i 1 nrot 0]
      (if (> i 50)
        nil
        (let [sm (loop [ip 0 s 0.0]
                     (if (>= ip (dec n)) s
                       (recur (inc ip)
                              (loop [iq (inc ip) s s]
                                (if (>= iq n) s
                                  (recur (inc iq) (+ s (absv (dget a ip iq)))))))))]
          (if (= sm 0.0)
            (do (eigsrt d v) [d v nrot])
            (let [tresh (if (< i 4) (/ (* 0.2 sm) (* n n)) 0.0)
                  nrot (loop [ip 0 nrot nrot]
                         (if (>= ip (dec n)) nrot
                           (recur (inc ip)
                                  (loop [iq (inc ip) nrot nrot]
                                    (if (>= iq n) nrot
                                      (let [g (* 100.0 (absv (dget a ip iq)))]
                                        (cond (and (> i 4)
                                                   (<= g (* eps (absv (aget ^doubles d ip))))
                                                   (<= g (* eps (absv (aget ^doubles d iq)))))
                                              (do (dset! a ip iq 0.0)
                                                  (recur (inc iq) nrot))
                                              (> (absv (dget a ip iq)) tresh)
                                              (let [h (- (aget ^doubles d iq)
                                                         (aget ^doubles d ip))
                                                    t (if (<= g (* eps (absv h)))
                                                        (/ (dget a ip iq) h)
                                                        (let [theta (/ (* 0.5 h) (dget a ip iq))
                                                              tt (/ 1.0 (+ (absv theta)
                                                                             (Math/sqrt
                                                                               (+ 1.0 (* theta theta)))))]
                                                          (if (< theta 0.0) (- tt) tt)))
                                                    c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                                                    s (* t c)
                                                    tau (/ s (+ 1.0 c))
                                                    h (* t (dget a ip iq))]
                                                (aset ^doubles z ip (- (aget ^doubles z ip) h))
                                                (aset ^doubles z iq (+ (aget ^doubles z iq) h))
                                                (aset ^doubles d ip (- (aget ^doubles d ip) h))
                                                (aset ^doubles d iq (+ (aget ^doubles d iq) h))
                                                (dset! a ip iq 0.0)
                                                (dotimes [j ip] (rot a s tau j ip j iq))
                                                (loop [j (inc ip)]
                                                  (when (< j iq)
                                                    (rot a s tau ip j j iq)
                                                    (recur (inc j))))
                                                (loop [j (inc iq)]
                                                  (when (< j n)
                                                    (rot a s tau ip j iq j)
                                                    (recur (inc j))))
                                                (dotimes [j n] (rot v s tau j ip j iq))
                                                (recur (inc iq) (inc nrot)))
                                              :else (recur (inc iq) nrot))))))))]
              (dotimes [ip n]
                (aset ^doubles b ip (+ (aget ^doubles b ip) (aget ^doubles z ip)))
                (aset ^doubles d ip (aget ^doubles b ip))
                (aset ^doubles z ip 0.0))
              (recur (inc i) nrot))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          vals (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (* n n)))))
          a (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                       (range n)))
          res (when (= op "JACOBI")
                (let [r (jacobi a)]
                  (if (nil? r)
                    (str (str/join " " (repeat (+ n (* n n)) "0.000000")) " 0 1")
                    (let [d (nth r 0)
                          v (nth r 1)
                          nrot (nth r 2)]
                      (str (str/join " "
                                     (concat (map #(fmt6 (canon (aget ^doubles d %))) (range n))
                                             (map #(fmt6 (canon (dget v (quot % n) (rem % n))))
                                                   (range (* n n)))))
                           " " nrot " 0")))))]
      (when res (println res)))))
