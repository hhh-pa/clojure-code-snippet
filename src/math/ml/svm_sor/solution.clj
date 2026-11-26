;; Support vector machines: Mangasarian-Musicant 1-norm soft-margin
;; variant solved by successive overrelaxation (SOR) with linear,
;; polynomial, and Gaussian kernels.
;; Reference: Press et al., Numerical Recipes 3e, Section 16.5
;; (Svm/Svmgenkernel from svm.h, vendoring Ran from 7.1 and
;; Indexx::index from 8.2).
;; Documented decisions: nrelax is an explicit input (fixed group
;; count, never data-dependent convergence); Ran(21) is reseeded
;; per SVMTRAIN exactly as the book's Svm constructor; Indexx is
;; the book's quicksort+insertion hybrid verbatim (its tie order
;; feeds the update sequence); Svm::relax updates alph[j] with
;; (om/(ker+1))*(y*sum-1) and projection MAX(0,MIN(lambda,.));
;; y values are used as given (the book assumes +-1 without
;; checking); m, n >= 1 and 1 <= nrelax <= 100000 enforced,
;; m <= 2048 and n <= 4096 bound the kernel matrix (robustness
;; caps; the book checks none of these); unknown kernels flag.
;; No online-judge problem found -- fixtures are self-generated
;; (KKT/formula oracle), no header link.
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

;; Ran (NR 7.1): wrapping 64-bit arithmetic needs unchecked ops
;; (checked * and + throw on overflow) and unsigned shifts.
(defn make-ran [j]
  (let [st (atom {:u 0 :v 4101842887655102017 :w 1})]
    (swap! st assoc :u (bit-xor j (:v @st)))
    (letfn [(step []
              (let [{:keys [u v w]} @st
                    u (unchecked-add (unchecked-multiply u 2862933555777941757)
                                     7046029254386353087)
                    v1 (bit-xor v (unsigned-bit-shift-right v 17))
                    v2 (bit-xor v1 (bit-shift-left v1 31))
                    v (bit-xor v2 (unsigned-bit-shift-right v2 8))
                    w (unchecked-add (unchecked-multiply 4294957665 (bit-and w 0xFFFFFFFF))
                                     (unsigned-bit-shift-right w 32))
                    x1 (bit-xor u (bit-shift-left u 21))
                    x2 (bit-xor x1 (unsigned-bit-shift-right x1 35))
                    x (bit-xor x2 (bit-shift-left x2 4))
                    r (bit-xor (unchecked-add x v) w)]
                (swap! st assoc :u u :v v :w w)
                r))]
      (step)
      (swap! st assoc :v (:u @st))
      (step)
      (swap! st assoc :w (:v @st))
      (step)
      {:state st :step step})))

(defn ran-int64 [r] ((:step r)))

(defn ran-int32 [r] (bit-and (ran-int64 r) 0xFFFFFFFF))

(defn make-kernel [kind data params]
  (let [m (count data)
        n (count (first data))
        mu (double-array n)
        _ (dotimes [i m]
            (dotimes [j n]
              (aset mu j (+ (aget mu j) (nth (nth data i) j)))))]
    (dotimes [j n]
      (aset mu j (/ (aget mu j) (double m))))
    (let [pa (nth params 0) pb (nth params 1) pd (nth params 2) ps (nth params 3)
          call (fn [xi xj]
                 (cond (= kind "LINEAR")
                       (loop [k 0 dott 0.0]
                         (if (>= k n) dott
                           (recur (inc k) (+ dott (* (- (nth xi k) (aget mu k))
                                                     (- (nth xj k) (aget mu k)))))))
                       (= kind "POLY")
                       (let [dott (loop [k 0 dott 0.0]
                                    (if (>= k n) dott
                                      (recur (inc k) (+ dott (* (nth xi k) (nth xj k))))))]
                         (Math/pow (+ (* pa dott) pb) pd))
                       :else
                       (let [dott (loop [k 0 dott 0.0]
                                    (if (>= k n) dott
                                      (recur (inc k) (+ dott (* (- (nth xi k) (nth xj k))
                                                                 (- (nth xi k) (nth xj k)))))))]
                         (Math/exp (/ (* -0.5 dott) (* ps ps))))))
          ker0 (vec (map vec (partition m (repeat (* m m) 0.0))))
          ker (atom ker0)]
      (dotimes [i m]
        (dotimes [j (inc i)]
          (let [v (call (nth data i) (nth data j))]
            (swap! ker assoc-in [i j] v)
            (swap! ker assoc-in [j i] v))))
      {:kind kind :m m :n n :mu mu :data data :ker @ker :call call})))

(defn kern-at [kern i j] (nth (nth (:ker kern) i) j))

(defn kcall-jx [kern j x] ((:call kern) (nth (:data kern) j) x))

(defn indexx [^doubles arr]
  (let [m 7
        nstack 64
        n (alength ^doubles arr)
        indx (int-array (range n))
        istack (int-array nstack)
        st (atom {:ir (dec n) :l 0 :jstack -1})]
    (loop []
      (let [{:keys [ir l jstack]} @st]
        (if (< (- ir l) m)
          (do
            (loop [j (inc l)]
              (when (<= j ir)
                (let [indxt (aget ^ints indx j)
                      a (aget ^doubles arr indxt)]
                  (loop [i (dec j)]
                    (if (and (>= i l)
                             (> (aget ^doubles arr (aget ^ints indx i)) a))
                      (do (aset ^ints indx (inc i) (aget ^ints indx i))
                          (recur (dec i)))
                      (aset ^ints indx (inc i) indxt))))
                (recur (inc j))))
            (if (< jstack 0)
              indx
              (let [ir (aget ^ints istack jstack)
                    l (aget ^ints istack (dec jstack))]
                (swap! st assoc :ir ir :l l :jstack (- jstack 2))
                (recur))))
          (let [k (bit-shift-right (+ l ir) 1)]
            (let [t (aget ^ints indx k)]
              (aset ^ints indx k (aget ^ints indx (inc l)))
              (aset ^ints indx (inc l) t))
            (when (> (aget ^doubles arr (aget ^ints indx l))
                     (aget ^doubles arr (aget ^ints indx ir)))
              (let [t (aget ^ints indx l)]
                (aset ^ints indx l (aget ^ints indx ir))
                (aset ^ints indx ir t)))
            (when (> (aget ^doubles arr (aget ^ints indx (inc l)))
                     (aget ^doubles arr (aget ^ints indx ir)))
              (let [t (aget ^ints indx (inc l))]
                (aset ^ints indx (inc l) (aget ^ints indx ir))
                (aset ^ints indx ir t)))
            (when (> (aget ^doubles arr (aget ^ints indx l))
                     (aget ^doubles arr (aget ^ints indx (inc l))))
              (let [t (aget ^ints indx l)]
                (aset ^ints indx l (aget ^ints indx (inc l)))
                (aset ^ints indx (inc l) t)))
            (let [indxt (aget ^ints indx (inc l))
                  a (aget ^doubles arr indxt)
                  [i j] (loop [i (inc l) j ir]
                          (let [i (loop [i (inc i)]
                                    (if (< (aget ^doubles arr (aget ^ints indx i)) a)
                                      (recur (inc i))
                                      i))
                                j (loop [j (dec j)]
                                    (if (> (aget ^doubles arr (aget ^ints indx j)) a)
                                      (recur (dec j))
                                      j))]
                            (if (< j i)
                              [i j]
                              (do
                                (let [t (aget ^ints indx i)]
                                  (aset ^ints indx i (aget ^ints indx j))
                                  (aset ^ints indx j t))
                                (recur i j)))))]
              (aset ^ints indx (inc l) (aget ^ints indx j))
              (aset ^ints indx j indxt)
              (let [jstack (+ jstack 2)]
                (when (>= jstack nstack)
                  (throw (ex-info "NSTACK" {})))
                (if (>= (+ (- ir i) 1) (- j l))
                  (do (aset ^ints istack jstack ir)
                      (aset ^ints istack (dec jstack) i)
                      (swap! st assoc :ir (dec j) :l l :jstack jstack))
                  (do (aset ^ints istack jstack (dec j))
                      (aset ^ints istack (dec jstack) l)
                      (swap! st assoc :ir ir :l i :jstack jstack)))
                (recur)))))))))

(defn svm-relax [svm kern ^doubles y lam om]
  (let [m (:m svm)
        ^doubles alph (:alph svm)
        ^doubles alphold (:alphold svm)
        r (:ran svm)]
    (when-not (:init @(:state svm))
      (dotimes [j m] (aset ^doubles alph j 0.0))
      (swap! (:state svm) assoc :init true))
    (dotimes [j m] (aset ^doubles alphold j (aget ^doubles alph j)))
    (let [x (indexx alph)
          fnz (loop [f 0]
                (if (or (>= f m) (not= (aget ^doubles alph (aget ^ints x f)) 0.0))
                  f (recur (inc f))))]
      (loop [j fnz]
        (when (< j (- m 2))
          (let [k (+ j (rem (ran-int32 r) (- m j)))
                t (aget ^ints x j)]
            (aset ^ints x j (aget ^ints x k))
            (aset ^ints x k t))
          (recur (inc j))))
      (let [_fnz (loop [jj 0 fnz fnz]
                   (if (>= jj m)
                     fnz
                    (let [j (aget ^ints x jj)
                          s (loop [kk fnz s 0.0]
                              (if (>= kk m) s
                                (let [k (aget ^ints x kk)]
                                  (recur (inc kk)
                                         (+ s (* (* (+ (kern-at kern j k) 1.0)
                                                     (aget ^doubles y k))
                                                  (aget ^doubles alph k)))))))
                          nv (- (aget ^doubles alph j)
                                (* (/ om (+ (kern-at kern j j) 1.0))
                                   (- (* (aget ^doubles y j) s) 1.0)))
                          nv (max 0.0 (min lam nv))]
                      (aset ^doubles alph j nv)
                      (if (and (< jj fnz) (not= nv 0.0))
                        (let [fnz (dec fnz)
                              t (aget ^ints x fnz)]
                          (aset ^ints x fnz (aget ^ints x jj))
                          (aset ^ints x jj t)
                          (recur (inc jj) fnz))
                        (recur (inc jj) fnz)))))]
        (let [yi (indexx alph)
              fnz (loop [f 0]
                    (if (or (>= f m) (not= (aget ^doubles alph (aget ^ints yi f)) 0.0))
                      f (recur (inc f))))
              fub (loop [f fnz]
                    (if (or (>= f m) (= (aget ^doubles alph (aget ^ints yi f)) lam))
                      f (recur (inc f))))]
          (loop [j fnz]
            (when (< j (- fub 2))
              (let [k (+ j (rem (ran-int32 r) (- fub j)))
                    t (aget ^ints yi j)]
                (aset ^ints yi j (aget ^ints yi k))
                (aset ^ints yi k t))
              (recur (inc j))))
          (let [pinsum (double-array m)]
            (loop [jj fnz]
              (when (< jj fub)
                (let [j (aget ^ints yi jj)
                      s (loop [kk fub s 0.0]
                          (if (>= kk m) s
                            (let [k (aget ^ints yi kk)]
                              (recur (inc kk)
                                     (+ s (* (* (+ (kern-at kern j k) 1.0)
                                                 (aget ^doubles y k))
                                              (aget ^doubles alph k)))))))]
                  (aset ^doubles pinsum jj s))
                (recur (inc jj))))
            (let [denom (double (+ (- fub fnz) 1))
                  niter (max (long (/ (* (* 0.5 (+ (double m) 1.0))
                                          (+ (- (double m) (double fnz)) 1.0))
                                       (* denom denom))) 1)]
              (dotimes [_ niter]
                (loop [jj fnz]
                  (when (< jj fub)
                    (let [j (aget ^ints yi jj)
                          s (loop [kk fnz s (aget ^doubles pinsum jj)]
                              (if (>= kk fub) s
                                (let [k (aget ^ints yi kk)]
                                  (recur (inc kk)
                                         (+ s (* (* (+ (kern-at kern j k) 1.0)
                                                     (aget ^doubles y k))
                                                  (aget ^doubles alph k)))))))
                          nv (- (aget ^doubles alph j)
                                (* (/ om (+ (kern-at kern j j) 1.0))
                                   (- (* (aget ^doubles y j) s) 1.0)))
                          nv (max 0.0 (min lam nv))]
                      (aset ^doubles alph j nv))
                    (recur (inc jj))))))))
        (let [dalph (loop [j 0 d 0.0]
                      (if (>= j m) d
                        (let [dd (- (aget ^doubles alph j) (aget ^doubles alphold j))]
                          (recur (inc j) (+ d (* dd dd))))))]
          (Math/sqrt dalph))))))

(defn make-svm [m]
  {:m m :alph (double-array m) :alphold (double-array m)
   :ran (make-ran 21) :state (atom {:init false})})

(defn svm-predict-k [svm kern ^doubles y kk]
  (let [m (:m svm)
        ^doubles alph (:alph svm)]
    (loop [j 0 s 0.0]
      (if (>= j m) s
        (recur (inc j) (+ s (* (* (aget ^doubles alph j) (aget ^doubles y j))
                                 (+ (kern-at kern j kk) 1.0))))))))

(defn svm-predict-x [svm kern ^doubles y x]
  (let [m (:m svm)
        ^doubles alph (:alph svm)]
    (loop [j 0 s 0.0]
      (if (>= j m) s
        (recur (inc j) (+ s (* (* (aget ^doubles alph j) (aget ^doubles y j))
                                 (+ (kcall-jx kern j x) 1.0))))))))

(defn emit [vals flag]
  (println (str (str/join " " (concat (map #(fmt6 (canon %)) vals) [(str flag)])))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (when (or (= op "SVMTRAIN") (= op "SVMPREDICT"))
        (let [ks (nth toks 1)
              [kind params pos]
              (cond (= ks "LINEAR") ["LINEAR" [0.0 0.0 0.0 0.0] 2]
                    (= ks "POLY") ["POLY" [(Double/parseDouble (nth toks 2))
                                           (Double/parseDouble (nth toks 3))
                                           (Double/parseDouble (nth toks 4)) 0.0] 5]
                    (= ks "GAUSS") ["GAUSS" [0.0 0.0 0.0 (Double/parseDouble (nth toks 2))] 3]
                    :else [nil [0.0 0.0 0.0 0.0] 2])]
          (cond
            (= op "SVMTRAIN")
            (let [lam (Double/parseDouble (nth toks pos))
                  om (Double/parseDouble (nth toks (+ pos 1)))
                  nrelax (Long/parseLong (nth toks (+ pos 2)))
                  m (Long/parseLong (nth toks (+ pos 3)))
                  n (Long/parseLong (nth toks (+ pos 4)))
                  pos (+ pos 5)
                  shapeok (and (>= m 1) (<= m 2048) (>= n 1) (<= n 4096))
                  ok (and shapeok (some? kind) (>= nrelax 1) (<= nrelax 100000))]
              (if ok
                (let [data (vec (map vec (partition n (map #(Double/parseDouble %)
                                                                (subvec toks pos (+ pos (* m n)))))))
                      y (double-array (map #(Double/parseDouble %)
                                           (subvec toks (+ pos (* m n)) (+ pos (* m n) m))))
                      kern (make-kernel kind data params)
                      svm (make-svm m)]
                  (loop [r 0 dalph 0.0]
                    (if (>= r nrelax)
                      (emit (concat (seq (:alph svm)) [dalph]
                                    (map #(svm-predict-k svm kern y %) (range m))) 0)
                      (recur (inc r) (svm-relax svm kern y lam om)))))
                (emit (repeat (if shapeok (+ (* 2 m) 1) 0) 0.0) 1)))
            (= op "SVMPREDICT")
            (let [m (Long/parseLong (nth toks pos))
                  n (Long/parseLong (nth toks (+ pos 1)))
                  pos (+ pos 2)
                  shapeok (and (>= m 1) (<= m 2048) (>= n 1) (<= n 4096))
                  ok (and shapeok (some? kind))]
              (if ok
                (let [data (vec (map vec (partition n (map #(Double/parseDouble %)
                                                                (subvec toks pos (+ pos (* m n)))))))
                      pos (+ pos (* m n))
                      y (double-array (map #(Double/parseDouble %)
                                           (subvec toks pos (+ pos m))))
                      pos (+ pos m)
                      alph (double-array (map #(Double/parseDouble %)
                                              (subvec toks pos (+ pos m))))
                      pos (+ pos m)
                      x (vec (map #(Double/parseDouble %)
                                  (subvec toks pos (+ pos n))))
                      kern (make-kernel kind data params)
                      svm (assoc (make-svm m) :alph alph)]
                  (swap! (:state svm) assoc :init true)
                  (emit [(svm-predict-x svm kern y x)] 0))
                (emit (repeat (if shapeok 1 0) 0.0) 1)))))))))
