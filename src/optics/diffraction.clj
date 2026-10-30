;; diffraction: Fraunhofer & Fresnel scalar diffraction via radix-2 FFT —
;; aperture samples (Fresnel premultiplied by the chirp exp(i pi x^2 /
;; lambda z)) are Fourier transformed; peak-normalized intensities.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn sn [x] (if (< (Math/abs (double x)) 5e-7) 0.0 x))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [PI Math/PI
            N (Long/parseLong (nth toks 0)) mode (Long/parseLong (nth toks 1))
            A (mapv (fn [j] (Double/parseDouble (nth toks (+ 2 j)))) (range N))
            lam (if (= mode 1) (Double/parseDouble (nth toks (+ 2 N))) 1.0)
            z (if (= mode 1) (Double/parseDouble (nth toks (+ 3 N))) 1.0)
            dx (if (= mode 1) (Double/parseDouble (nth toks (+ 4 N))) 1.0)
            re0 (mapv (fn [j] (if (= mode 0) (nth A j)
                               (let [x (* (- j (/ N 2.0)) dx) ph (/ (* PI x x) (* lam z))]
                                 (* (nth A j) (Math/cos ph))))) (range N))
            im0 (mapv (fn [j] (if (= mode 0) 0.0
                               (let [x (* (- j (/ N 2.0)) dx) ph (/ (* PI x x) (* lam z))]
                                 (* (nth A j) (Math/sin ph))))) (range N))
            [re1 im1] (loop [i 1 kk 0 re re0 im im0]
                        (if (>= i N) [re im]
                          (let [b0 (bit-shift-right N 1)
                                kk2 (loop [b b0 k kk]
                                      (if (zero? (bit-and k b)) (bit-or k b)
                                        (recur (bit-shift-right b 1) (bit-and k (bit-not b)))))]
                            (if (< i kk2)
                              (recur (inc i) kk2 (assoc (assoc re i (nth re kk2)) kk2 (nth re i))
                                     (assoc (assoc im i (nth im kk2)) kk2 (nth im i)))
                              (recur (inc i) kk2 re im)))))
            [re2 im2] (loop [m 2 re re1 im im1]
                        (if (> m N) [re im]
                          (let [ang (/ (* -2 PI) m) wr (Math/cos ang) wi (Math/sin ang)
                                [re3 im3] (loop [k 0 re re im im]
                                            (if (>= k N) [re im]
                                              (let [[re4 im4] (loop [j 0 cr 1.0 ci 0.0 re re im im]
                                                                (if (>= j (/ m 2)) [re im]
                                                                  (let [a (+ k j) b (+ a (/ m 2))
                                                                        tr (- (* cr (nth re b)) (* ci (nth im b)))
                                                                        ti (+ (* cr (nth im b)) (* ci (nth re b)))
                                                                        re5 (assoc (assoc re b (- (nth re a) tr)) a (+ (nth re a) tr))
                                                                        im5 (assoc (assoc im b (- (nth im a) ti)) a (+ (nth im a) ti))
                                                                        nr (- (* cr wr) (* ci wi))
                                                                        ci2 (+ (* cr wi) (* ci wr))]
                                                                    (recur (inc j) nr ci2 re5 im5))))]
                                                (recur (+ k m) re4 im4))))]
                            (recur (* m 2) re3 im3))))
            O (mapv (fn [k] (+ (* (nth re2 k) (nth re2 k)) (* (nth im2 k) (nth im2 k)))) (range N))
            mx (reduce max O)]
        (println (str "I:" (apply str (map (fn [k] (fmt " %.6f" (sn (+ (/ (nth O k) mx) 0.0)))) (range N)))))))))
(-main (slurp *in*))
