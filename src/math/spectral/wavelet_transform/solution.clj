;; One- and multi-dimensional discrete wavelet transform (pyramid
;; algorithm) with Daubechies filters: Daub4, Daubs (DAUB4/12/20),
;; and Daub4i (DAUB4 on the interval with preconditioning).
;; Reference: Press et al., Numerical Recipes 3e, Section 13.11
;; (wt1/wtn/Daub4/Daubs/Daub4i from wavelet.h).
;; Documented decisions: n must be a power of 2 (the book's stated
;; precondition; its bitmask wraparound needs it); n = 1, 2 pass
;; through unchanged with flag 0 (the book's silent `n < 4`
;; return); Daub4i's filt likewise no-ops below n = 8; unknown
;; filter names flag (window-name precedent); wtn flags unless
;; every dim >= 1 and the product is a power of 2 (equivalent to
;; the book's product-only throw for positive dims); isign >= 0
;; transforms, isign < 0 inverts, exactly as the book; Daub4's
;; C1/C3 last digits differ from Daubs::c4's and are each kept
;; verbatim (0.8365163037378077 vs ...079, -0.1294095225512603
;; vs ...604).
;; No online-judge problem found -- fixtures are self-generated
;; (roundtrip/energy/hand-check oracle), no header link.
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

(defn ispow2 [n] (and (>= n 1) (zero? (bit-and n (dec n)))))

(def D4-C0 0.4829629131445341)
(def D4-C1 0.8365163037378077)
(def D4-C2 0.2241438680420134)
(def D4-C3 -0.1294095225512603)

(def C4 [0.4829629131445341 0.8365163037378079
         0.2241438680420134 -0.1294095225512604])
(def C12 [0.111540743350 0.494623890398 0.751133908021
          0.315250351709 -0.226264693965 -0.129766867567
          0.097501605587 0.027522865530 -0.031582039318
          0.000553842201 0.004777257511 -0.001077301085])
(def C20 [0.026670057901 0.188176800078 0.527201188932
          0.688459039454 0.281172343661 -0.249846424327
          -0.195946274377 0.127369340336 0.093057364604
          -0.071394147166 -0.029457536822 0.033212674059
          0.003606553567 -0.010733175483 0.001395351747
          0.001992405295 -0.000685856695 -0.000116466855
          0.000093588670 -0.000013264203])

(defn daub4-filt [^doubles a n isign]
  (when (>= n 4)
    (let [wksp (double-array n)
          nh (bit-shift-right n 1)]
      (if (>= isign 0)
        (let [i (loop [i 0 j 0]
                  (if (>= j (- n 3))
                    i
                    (do
                      (aset ^doubles wksp i
                            (+ (* D4-C0 (aget ^doubles a j))
                               (* D4-C1 (aget ^doubles a (+ j 1)))
                               (* D4-C2 (aget ^doubles a (+ j 2)))
                               (* D4-C3 (aget ^doubles a (+ j 3)))))
                      (aset ^doubles wksp (+ i nh)
                            (- (+ (- (* D4-C3 (aget ^doubles a j))
                                       (* D4-C2 (aget ^doubles a (+ j 1))))
                                    (* D4-C1 (aget ^doubles a (+ j 2))))
                               (* D4-C0 (aget ^doubles a (+ j 3)))))
                      (recur (inc i) (+ j 2)))))]
          (aset ^doubles wksp i
                (+ (* D4-C0 (aget ^doubles a (- n 2)))
                   (* D4-C1 (aget ^doubles a (- n 1)))
                   (* D4-C2 (aget ^doubles a 0))
                   (* D4-C3 (aget ^doubles a 1))))
          (aset ^doubles wksp (+ i nh)
                (- (+ (- (* D4-C3 (aget ^doubles a (- n 2)))
                           (* D4-C2 (aget ^doubles a (- n 1))))
                        (* D4-C1 (aget ^doubles a 0)))
                   (* D4-C0 (aget ^doubles a 1)))))
        (do
          (aset ^doubles wksp 0
                (+ (* D4-C2 (aget ^doubles a (- nh 1)))
                   (* D4-C1 (aget ^doubles a (- n 1)))
                   (* D4-C0 (aget ^doubles a 0))
                   (* D4-C3 (aget ^doubles a nh))))
          (aset ^doubles wksp 1
                (- (+ (- (* D4-C3 (aget ^doubles a (- nh 1)))
                           (* D4-C0 (aget ^doubles a (- n 1))))
                        (* D4-C1 (aget ^doubles a 0)))
                   (* D4-C2 (aget ^doubles a nh))))
          (loop [i 0 j 2]
            (when (< i (- nh 1))
              (aset ^doubles wksp j
                    (+ (* D4-C2 (aget ^doubles a i))
                       (* D4-C1 (aget ^doubles a (+ i nh)))
                       (* D4-C0 (aget ^doubles a (+ i 1)))
                       (* D4-C3 (aget ^doubles a (+ i nh 1)))))
              (aset ^doubles wksp (+ j 1)
                    (- (+ (- (* D4-C3 (aget ^doubles a i))
                               (* D4-C0 (aget ^doubles a (+ i nh))))
                            (* D4-C1 (aget ^doubles a (+ i 1))))
                       (* D4-C2 (aget ^doubles a (+ i nh 1)))))
              (recur (inc i) (+ j 2))))))
      (dotimes [i n]
        (aset ^doubles a i (aget ^doubles wksp i))))))

(defn daubs-make [n]
  (let [src (cond (= n 4) C4 (= n 12) C12 (= n 20) C20 :else nil)]
    (when src
      (let [cc (double-array src)
            cr (double-array n)]
        (loop [i 0 sig -1.0]
          (when (< i n)
            (aset ^doubles cr (- (dec n) i) (* sig (aget ^doubles cc i)))
            (recur (inc i) (- sig))))
        {:ncof n :ioff (- (bit-shift-right n 1))
         :joff (- (bit-shift-right n 1)) :cc cc :cr cr}))))

(defn daubs-filt [^doubles a n isign tab]
  (when (>= n 4)
    (let [{:keys [ncof ioff joff cc cr]} tab
          ^doubles cc cc
          ^doubles cr cr
          wksp (double-array n)
          nmod (* ncof n)
          n1 (dec n)
          nh (bit-shift-right n 1)]
      (if (>= isign 0)
        (loop [ii 0 i 0]
          (when (< i n)
            (let [ni (+ (+ i 1) nmod ioff)
                  nj (+ (+ i 1) nmod joff)]
              (dotimes [k ncof]
                (let [jf (bit-and n1 (+ (+ ni k) 1))
                      jr (bit-and n1 (+ (+ nj k) 1))]
                  (aset ^doubles wksp ii
                        (+ (aget ^doubles wksp ii)
                           (* (aget ^doubles cc k) (aget ^doubles a jf))))
                  (aset ^doubles wksp (+ ii nh)
                        (+ (aget ^doubles wksp (+ ii nh))
                           (* (aget ^doubles cr k) (aget ^doubles a jr)))))))
            (recur (inc ii) (+ i 2))))
        (loop [ii 0 i 0]
          (when (< i n)
            (let [ai (aget ^doubles a ii)
                  ai1 (aget ^doubles a (+ ii nh))
                  ni (+ (+ i 1) nmod ioff)
                  nj (+ (+ i 1) nmod joff)]
              (dotimes [k ncof]
                (let [jf (bit-and n1 (+ (+ ni k) 1))
                      jr (bit-and n1 (+ (+ nj k) 1))]
                  (aset ^doubles wksp jf
                        (+ (aget ^doubles wksp jf)
                           (* (aget ^doubles cc k) ai)))
                  (aset ^doubles wksp jr
                        (+ (aget ^doubles wksp jr)
                           (* (aget ^doubles cr k) ai1))))))
            (recur (inc ii) (+ i 2)))))
      (dotimes [j n]
        (aset ^doubles a j (aget ^doubles wksp j))))))

(def R00 0.603332511928053)
(def R01 0.690895531839104)
(def R02 -0.398312997698228)
(def R10 -0.796543516912183)
(def R11 0.546392713959015)
(def R12 -0.258792248333818)
(def R20 0.0375174604524466)
(def R21 0.457327659851769)
(def R22 0.850088102549165)
(def R23 0.223820356983114)
(def R24 -0.129222743354319)
(def R30 0.0100372245644139)
(def R31 0.122351043116799)
(def R32 0.227428111655837)
(def R33 -0.836602921223654)
(def R34 0.483012921773304)
(def R43 0.443149049637559)
(def R44 0.767556669298114)
(def R45 0.374955331645687)
(def R46 0.190151418429955)
(def R47 -0.194233407427412)
(def R53 0.231557595006790)
(def R54 0.401069519430217)
(def R55 -0.717579999353722)
(def R56 -0.363906959570891)
(def R57 0.371718966535296)
(def R65 0.230389043796969)
(def R66 0.434896997965703)
(def R67 0.870508753349866)
(def R75 -0.539822500731772)
(def R76 0.801422961990337)
(def R77 -0.257512919478482)

(defn daub4i-filt [^doubles a n isign]
  (when (>= n 8)
    (let [wksp (double-array n)
          nh (bit-shift-right n 1)]
      (if (>= isign 0)
        (do
          (aset ^doubles wksp 0 (+ (* R00 (aget ^doubles a 0))
                                    (* R01 (aget ^doubles a 1))
                                    (* R02 (aget ^doubles a 2))))
          (aset ^doubles wksp nh (+ (* R10 (aget ^doubles a 0))
                                      (* R11 (aget ^doubles a 1))
                                      (* R12 (aget ^doubles a 2))))
          (aset ^doubles wksp 1 (+ (* R20 (aget ^doubles a 0))
                                    (* R21 (aget ^doubles a 1))
                                    (* R22 (aget ^doubles a 2))
                                    (* R23 (aget ^doubles a 3))
                                    (* R24 (aget ^doubles a 4))))
          (aset ^doubles wksp (+ nh 1) (+ (* R30 (aget ^doubles a 0))
                                           (* R31 (aget ^doubles a 1))
                                           (* R32 (aget ^doubles a 2))
                                           (* R33 (aget ^doubles a 3))
                                           (* R34 (aget ^doubles a 4))))
          (loop [i 2 j 3]
            (when (< j (- n 4))
              (aset ^doubles wksp i
                    (+ (* D4-C0 (aget ^doubles a j))
                       (* D4-C1 (aget ^doubles a (+ j 1)))
                       (* D4-C2 (aget ^doubles a (+ j 2)))
                       (* D4-C3 (aget ^doubles a (+ j 3)))))
              (aset ^doubles wksp (+ i nh)
                    (- (+ (- (* D4-C3 (aget ^doubles a j))
                               (* D4-C2 (aget ^doubles a (+ j 1))))
                            (* D4-C1 (aget ^doubles a (+ j 2))))
                       (* D4-C0 (aget ^doubles a (+ j 3)))))
              (recur (inc i) (+ j 2))))
          (aset ^doubles wksp (- nh 2) (+ (* R43 (aget ^doubles a (- n 5)))
                                           (* R44 (aget ^doubles a (- n 4)))
                                           (* R45 (aget ^doubles a (- n 3)))
                                           (* R46 (aget ^doubles a (- n 2)))
                                           (* R47 (aget ^doubles a (- n 1)))))
          (aset ^doubles wksp (- n 2) (+ (* R53 (aget ^doubles a (- n 5)))
                                          (* R54 (aget ^doubles a (- n 4)))
                                          (* R55 (aget ^doubles a (- n 3)))
                                          (* R56 (aget ^doubles a (- n 2)))
                                          (* R57 (aget ^doubles a (- n 1)))))
          (aset ^doubles wksp (- nh 1) (+ (* R65 (aget ^doubles a (- n 3)))
                                           (* R66 (aget ^doubles a (- n 2)))
                                           (* R67 (aget ^doubles a (- n 1)))))
          (aset ^doubles wksp (- n 1) (+ (* R75 (aget ^doubles a (- n 3)))
                                          (* R76 (aget ^doubles a (- n 2)))
                                          (* R77 (aget ^doubles a (- n 1))))))
        (do
          (aset ^doubles wksp 0 (+ (* R00 (aget ^doubles a 0))
                                    (* R10 (aget ^doubles a nh))
                                    (* R20 (aget ^doubles a 1))
                                    (* R30 (aget ^doubles a (+ nh 1)))))
          (aset ^doubles wksp 1 (+ (* R01 (aget ^doubles a 0))
                                    (* R11 (aget ^doubles a nh))
                                    (* R21 (aget ^doubles a 1))
                                    (* R31 (aget ^doubles a (+ nh 1)))))
          (aset ^doubles wksp 2 (+ (* R02 (aget ^doubles a 0))
                                    (* R12 (aget ^doubles a nh))
                                    (* R22 (aget ^doubles a 1))
                                    (* R32 (aget ^doubles a (+ nh 1)))))
          (if (= n 8)
            (do
              (aset ^doubles wksp 3 (+ (* R23 (aget ^doubles a 1))
                                        (* R33 (aget ^doubles a 5))
                                        (* R43 (aget ^doubles a 2))
                                        (* R53 (aget ^doubles a 6))))
              (aset ^doubles wksp 4 (+ (* R24 (aget ^doubles a 1))
                                        (* R34 (aget ^doubles a 5))
                                        (* R44 (aget ^doubles a 2))
                                        (* R54 (aget ^doubles a 6)))))
            (do
              (aset ^doubles wksp 3 (+ (* R23 (aget ^doubles a 1))
                                        (* R33 (aget ^doubles a (+ nh 1)))
                                        (* D4-C0 (aget ^doubles a 2))
                                        (* D4-C3 (aget ^doubles a (+ nh 2)))))
              (aset ^doubles wksp 4 (- (+ (+ (* R24 (aget ^doubles a 1))
                                              (* R34 (aget ^doubles a (+ nh 1))))
                                           (* D4-C1 (aget ^doubles a 2)))
                                        (* D4-C2 (aget ^doubles a (+ nh 2)))))
              (aset ^doubles wksp (- n 5) (+ (* D4-C2 (aget ^doubles a (- nh 3)))
                                              (* D4-C1 (aget ^doubles a (- n 3)))
                                              (* R43 (aget ^doubles a (- nh 2)))
                                              (* R53 (aget ^doubles a (- n 2)))))
              (aset ^doubles wksp (- n 4)
                    (+ (+ (- (* D4-C3 (aget ^doubles a (- nh 3)))
                               (* D4-C0 (aget ^doubles a (- n 3))))
                            (* R44 (aget ^doubles a (- nh 2))))
                       (* R54 (aget ^doubles a (- n 2)))))))
          (loop [i 2 j 5]
            (when (< i (- nh 3))
              (aset ^doubles wksp j
                    (+ (* D4-C2 (aget ^doubles a i))
                       (* D4-C1 (aget ^doubles a (+ i nh)))
                       (* D4-C0 (aget ^doubles a (+ i 1)))
                       (* D4-C3 (aget ^doubles a (+ i nh 1)))))
              (aset ^doubles wksp (+ j 1)
                    (- (+ (- (* D4-C3 (aget ^doubles a i))
                               (* D4-C0 (aget ^doubles a (+ i nh))))
                            (* D4-C1 (aget ^doubles a (+ i 1))))
                       (* D4-C2 (aget ^doubles a (+ i nh 1)))))
              (recur (inc i) (+ j 2))))
          (aset ^doubles wksp (- n 3) (+ (* R45 (aget ^doubles a (- nh 2)))
                                          (* R55 (aget ^doubles a (- n 2)))
                                          (* R65 (aget ^doubles a (- nh 1)))
                                          (* R75 (aget ^doubles a (- n 1)))))
          (aset ^doubles wksp (- n 2) (+ (* R46 (aget ^doubles a (- nh 2)))
                                          (* R56 (aget ^doubles a (- n 2)))
                                          (* R66 (aget ^doubles a (- nh 1)))
                                          (* R76 (aget ^doubles a (- n 1)))))
          (aset ^doubles wksp (- n 1) (+ (* R47 (aget ^doubles a (- nh 2)))
                                          (* R57 (aget ^doubles a (- n 2)))
                                          (* R67 (aget ^doubles a (- nh 1)))
                                          (* R77 (aget ^doubles a (- n 1)))))))
      (dotimes [i n]
        (aset ^doubles a i (aget ^doubles wksp i))))))

(defn daub4i-condition [^doubles a n isign]
  (when (>= n 4)
    (if (>= isign 0)
      (let [t0 (+ (* 0.324894048898962 (aget ^doubles a 0))
                  (* 0.0371580151158803 (aget ^doubles a 1)))
            t1 (* 1.00144540498130 (aget ^doubles a 1))
            t2 (* 1.08984305289504 (aget ^doubles a (- n 2)))
            t3 (+ (* -0.800813234246437 (aget ^doubles a (- n 2)))
                  (* 2.09629288435324 (aget ^doubles a (- n 1))))]
        (aset ^doubles a 0 t0)
        (aset ^doubles a 1 t1)
        (aset ^doubles a (- n 2) t2)
        (aset ^doubles a (- n 1) t3))
      (let [t0 (- (* 3.07792649138669 (aget ^doubles a 0))
                  (* 0.114204567242137 (aget ^doubles a 1)))
            t1 (* 0.998556681198888 (aget ^doubles a 1))
            t2 (* 0.917563310922261 (aget ^doubles a (- n 2)))
            t3 (+ (* 0.350522032550918 (aget ^doubles a (- n 2)))
                  (* 0.477032578540915 (aget ^doubles a (- n 1))))]
        (aset ^doubles a 0 t0)
        (aset ^doubles a 1 t1)
        (aset ^doubles a (- n 2) t2)
        (aset ^doubles a (- n 1) t3)))))

(defn apply-filt [^doubles a n isign f tab]
  (cond (= f "DAUB4") (daub4-filt a n isign)
        (= f "DAUB4I") (daub4i-filt a n isign)
        :else (when tab (daubs-filt a n isign tab))))

(defn apply-condition [^doubles a n isign f]
  (when (= f "DAUB4I")
    (daub4i-condition a n isign)))

(defn wt1 [^doubles a isign f tab]
  (let [n (alength ^doubles a)]
    (when (>= n 4)
      (if (>= isign 0)
        (do
          (apply-condition a n 1 f)
          (loop [nn n]
            (when (>= nn 4)
              (apply-filt a nn isign f tab)
              (recur (bit-shift-right nn 1)))))
        (do
          (loop [nn 4]
            (when (<= nn n)
              (apply-filt a nn isign f tab)
              (recur (bit-shift-left nn 1))))
          (apply-condition a n -1 f))))))

(defn wtn [^doubles a dims isign f tab]
  (let [ntot (reduce * dims)]
    (when (ispow2 ntot)
      (loop [di 0 nprev 1]
        (when (< di (count dims))
          (let [n (nth dims di)
                wksp (double-array n)
                nnew (* n nprev)]
            (when (> n 4)
              (loop [i2 0]
                (when (< i2 ntot)
                  (dotimes [i1 nprev]
                    (loop [i3 (+ i1 i2) k 0]
                      (when (< k n)
                        (aset ^doubles wksp k (aget ^doubles a i3))
                        (recur (+ i3 nprev) (inc k))))
                    (if (>= isign 0)
                      (do
                        (apply-condition wksp n 1 f)
                        (loop [nt n]
                          (when (>= nt 4)
                            (apply-filt wksp nt isign f tab)
                            (recur (bit-shift-right nt 1)))))
                      (do
                        (loop [nt 4]
                          (when (<= nt n)
                            (apply-filt wksp nt isign f tab)
                            (recur (bit-shift-left nt 1))))
                        (apply-condition wksp n -1 f)))
                    (loop [i3 (+ i1 i2) k 0]
                      (when (< k n)
                        (aset ^doubles a i3 (aget ^doubles wksp k))
                        (recur (+ i3 nprev) (inc k)))))
                  (recur (+ i2 nnew)))))
            (recur (inc di) nnew))))
      true)))

(defn make-tab [f]
  (cond (= f "DAUBS4") (daubs-make 4)
        (= f "DAUBS12") (daubs-make 12)
        (= f "DAUBS20") (daubs-make 20)
        :else nil))

(defn known-filter [f]
  (contains? #{"DAUB4" "DAUBS4" "DAUBS12" "DAUBS20" "DAUB4I"} f))

(defn emit [^doubles a flag]
  (println (str (str/join " " (concat (map #(fmt6 (canon (aget ^doubles a %)))
                                            (range (alength ^doubles a)))
                                       [(str flag)])))))

(defn emit-zeros [n flag]
  (println (str (str/join " " (concat (repeat n "0.000000") [(str flag)])))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "WT1")
        (let [n (Long/parseLong (nth toks 1))
              f (nth toks 2)
              isign (Long/parseLong (nth toks 3))
              shapeok (and (>= n 1) (<= n 1048576))
              ok (and shapeok (known-filter f) (ispow2 n))]
          (if ok
            (let [a (double-array (map #(Double/parseDouble %)
                                       (subvec toks 4 (+ 4 n))))]
              (wt1 a isign f (make-tab f))
              (emit a 0))
            (emit-zeros (if shapeok n 0) 1)))
        (= op "WTN")
        (let [ndim (Long/parseLong (nth toks 1))
              nd (max ndim 0)
              dims (vec (map #(Long/parseLong %) (subvec toks 2 (+ 2 nd))))
              f (nth toks (+ 2 nd))
              isign (Long/parseLong (nth toks (+ 3 nd)))
              [dimsok prod] (if (< ndim 1)
                                [false 1]
                                (loop [ds dims prod 1]
                                  (if (empty? ds)
                                    [true prod]
                                    (let [d (first ds)]
                                      (if (or (< d 1) (> prod (quot 1048576 d)))
                                        [false prod]
                                        (recur (rest ds) (* prod d)))))))
              ok (and dimsok (known-filter f) (ispow2 prod))]
          (if ok
            (let [a (double-array (map #(Double/parseDouble %)
                                       (subvec toks (+ 4 nd) (+ 4 nd prod))))]
              (if (wtn a dims isign f (make-tab f))
                (emit a 0)
                (emit-zeros prod 1)))
            (emit-zeros (if dimsok prod 0) 1)))))))
