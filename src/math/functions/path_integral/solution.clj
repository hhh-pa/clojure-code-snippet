;; Hypergeometric 2F1 by path integration (NR 5.14).
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

(defn deriv [a b c zr zi dzr dzi fr fi gr gi]
  (let [dfr (- (* dzr gr) (* dzi gi))
        dfi (+ (* dzr gi) (* dzi gr))
        abr (* a b)
        kr (- c (* (+ a b 1.0) zr))
        ki (* (- (+ a b 1.0)) zi)
        t1r (* abr fr)
        t1i (* abr fi)
        t2r (- (* kr gr) (* ki gi))
        t2i (+ (* kr gi) (* ki gr))
        nr (- t1r t2r)
        ni (- t1i t2i)
        dr (+ (* zr (- 1.0 zr)) (* zi zi))
        di (- (* zi (- 1.0 zr)) (* zr zi))
        dd (+ (* dr dr) (* di di))
        qr (/ (+ (* nr dr) (* ni di)) dd)
        qi (/ (- (* ni dr) (* nr di)) dd)]
    [dfr dfi (- (* dzr qr) (* dzi qi)) (+ (* dzr qi) (* dzi qr))]))

(defn series [a b c zr zi]
  (loop [j 1 pr 1.0 pi 0.0 fr 1.0 fi 0.0 gr 0.0 gi 0.0]
    (if (> j 60) [fr fi gr gi]
        (let [f (/ (* (+ a j -1.0) (+ b j -1.0)) (* (+ c j -1.0) j))
              jf (* j f)]
          (recur (inc j)
                 (* (- (* pr zr) (* pi zi)) f)
                 (* (+ (* pr zi) (* pi zr)) f)
                 (+ fr (* (- (* pr zr) (* pi zi)) f))
                 (+ fi (* (+ (* pr zi) (* pi zr)) f))
                 (+ gr (* jf pr))
                 (+ gi (* jf pi)))))))

(defn rk4-step [a b c dzr dzi h cr ci fr fi gr gi]
  (let [[k1fr k1fi k1gr k1gi] (deriv a b c cr ci dzr dzi fr fi gr gi)
        mr (+ cr (* 0.5 h dzr))
        mi (+ ci (* 0.5 h dzi))
        [k2fr k2fi k2gr k2gi] (deriv a b c mr mi dzr dzi
                                    (+ fr (* 0.5 h k1fr)) (+ fi (* 0.5 h k1fi))
                                    (+ gr (* 0.5 h k1gr)) (+ gi (* 0.5 h k1gi)))
        [k3fr k3fi k3gr k3gi] (deriv a b c mr mi dzr dzi
                                    (+ fr (* 0.5 h k2fr)) (+ fi (* 0.5 h k2fi))
                                    (+ gr (* 0.5 h k2gr)) (+ gi (* 0.5 h k2gi)))
        er (+ cr (* h dzr))
        ei (+ ci (* h dzi))
        [k4fr k4fi k4gr k4gi] (deriv a b c er ei dzr dzi
                                    (+ fr (* h k3fr)) (+ fi (* h k3fi))
                                    (+ gr (* h k3gr)) (+ gi (* h k3gi)))]
    [(+ fr (/ (* h (+ k1fr (* 2.0 k2fr) (* 2.0 k3fr) k4fr)) 6.0))
     (+ fi (/ (* h (+ k1fi (* 2.0 k2fi) (* 2.0 k3fi) k4fi)) 6.0))
     (+ gr (/ (* h (+ k1gr (* 2.0 k2gr) (* 2.0 k3gr) k4gr)) 6.0))
     (+ gi (/ (* h (+ k1gi (* 2.0 k2gi) (* 2.0 k3gi) k4gi)) 6.0))
     er ei]))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [a (Double/parseDouble (nth toks 0))
          b (Double/parseDouble (nth toks 1))
          c (Double/parseDouble (nth toks 2))
          zr (Double/parseDouble (nth toks 3))
          zi (Double/parseDouble (nth toks 4))
          r (Math/sqrt (+ (* zr zr) (* zi zi)))
          [fr fi] (if (<= r 0.5)
                    (let [[fr fi _ _] (series a b c zr zi)] [fr fi])
                    (let [[z0r z0i] (cond (neg? zr) [-0.5 0.0]
                                          (<= zr 1.0) [0.5 0.0]
                                          (>= zi 0.0) [0.0 0.5]
                                          :else [0.0 -0.5])
                          [fr fi gr gi] (series a b c z0r z0i)
                          dzr (- zr z0r)
                          dzi (- zi z0i)
                          h (/ 1.0 800)]
                      (loop [s 0 cr z0r ci z0i fr fr fi fi gr gr gi gi]
                        (if (>= s 800) [fr fi]
                            (let [[fr fi gr gi er ei] (rk4-step a b c dzr dzi h cr ci fr fi gr gi)]
                              (recur (inc s) er ei fr fi gr gi))))))]
      (println (fmt6 (canon fr)) (fmt6 (canon fi))))))
