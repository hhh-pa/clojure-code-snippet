;; Pupils, windows, vignetting (geom ch 5).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- snap [x] (if (< (Math/abs x) 5e-7) 0.0 x))

(defn- image-of
  [a b c d z-end rim forward]
  (let [sp (cond (not (== d 0.0)) (if forward (/ (- b) d) (/ b d))
                 (> b 0.0) 1e15
                 (< b 0.0) -1e15
                 :else 1e15)]
    (if forward
      [(+ z-end sp) (* (Math/abs (+ a (* sp c))) rim)]
      [(- z-end sp) (* (Math/abs (- a (* sp c))) rim)])))

(defn- run-chain
  [zs fs order zstart forward]
  (loop [idx 0 a 1.0 b 0.0 c 0.0 d 1.0 zcur zstart]
    (if (>= idx (count order))
      [a b c d zcur]
      (let [i (nth order idx)
            dz (- (nth zs i) zcur)
            a2 (+ a (* c dz))
            b2 (+ b (* d dz))
            [c2 d2] (if (== (nth fs i) 0.0)
                      [c d]
                      (if forward
                        [(- c (/ a2 (nth fs i))) (- d (/ b2 (nth fs i)))]
                        [(+ c (/ a2 (nth fs i))) (+ d (/ b2 (nth fs i)))]))]
        (recur (inc idx) a2 b2 c2 d2 (nth zs i))))))

(defn- vignette
  [zs fs aps ho zo s]
  (loop [i 0 uc (/ (- 0.0 ho) (- (nth zs s) zo))
         up (/ (- (nth aps s) ho) (- (nth zs s) zo))
         um (/ (- (- (nth aps s)) ho) (- (nth zs s) zo))
         hc ho hp ho hm ho zcur zo vig 1.0]
    (if (>= i (count zs))
      (max 0.0 (min 1.0 vig))
      (let [dz (- (nth zs i) zcur)
            hc2 (+ hc (* uc dz))
            hp2 (+ hp (* up dz))
            hm2 (+ hm (* um dz))
            bw (max (Math/abs (- hp2 hc2)) (Math/abs (- hm2 hc2)))
            f (if (== bw 0.0) 1.0 (/ (- (nth aps i) (Math/abs hc2)) bw))
            vig2 (if (< f vig) f vig)
            [uc2 up2 um2] (if (== (nth fs i) 0.0)
                            [uc up um]
                            [(- uc (/ hc2 (nth fs i)))
                             (- up (/ hp2 (nth fs i)))
                             (- um (/ hm2 (nth fs i)))])]
        (recur (inc i) uc2 up2 um2 hc2 hp2 hm2 (nth zs i) vig2)))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          ho (Double/parseDouble (nth toks 0))
          zo (Double/parseDouble (nth toks 1))
          n (Long/parseLong (nth toks 2))
          zs (mapv #(Double/parseDouble (nth toks (+ 3 (* 3 %)))) (range n))
          fs (mapv #(Double/parseDouble (nth toks (+ 4 (* 3 %)))) (range n))
          aps (mapv #(Double/parseDouble (nth toks (+ 5 (* 3 %)))) (range n))
          st (Long/parseLong (nth toks (+ 3 (* 3 n))))
          w (Long/parseLong (nth toks (+ 4 (* 3 n))))
          back (vec (range (dec st) -1 -1))
          fwd (vec (range (inc st) n))
          wback (vec (range (dec w) -1 -1))
          [a b c d zcur] (run-chain zs fs back (nth zs st) false)
          [enz enr] (image-of a b c d zcur (nth aps st) false)
          [a b c d zcur] (run-chain zs fs fwd (nth zs st) true)
          [exz exr] (image-of a b c d zcur (nth aps st) true)
          [a b c d zcur] (run-chain zs fs wback (nth zs w) false)
          [wnz wnr] (image-of a b c d zcur (nth aps w) false)
          vig (vignette zs fs aps ho zo st)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "ENPUPIL " (fmt (snap enz)) " " (fmt (snap enr))
                  "\nEXPUPIL " (fmt (snap exz)) " " (fmt (snap exr))
                  "\nENWINDOW " (fmt (snap wnz)) " " (fmt (snap wnr))
                  "\nVIG " (fmt (snap vig)))))))
