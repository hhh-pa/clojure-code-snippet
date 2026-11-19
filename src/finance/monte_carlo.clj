;; Monte Carlo GBM option pricing (FRM ch 4).
(require '[clojure.string :as str])
(import '[java.util Locale])

(def ^:const LCG-A 6364136223846793005)
(def ^:const LCG-C 1442695040888963407)
(def ^:const DEN 9007199254740992.0)

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          s0 (Double/parseDouble (nth toks 0))
          k (Double/parseDouble (nth toks 1))
          t (Double/parseDouble (nth toks 2))
          r (Double/parseDouble (nth toks 3))
          sg (Double/parseDouble (nth toks 4))
          m (Long/parseLong (nth toks 5))
          seed (Long/parseLong (nth toks 6))
          step (fn [st] (unchecked-add (unchecked-multiply st LCG-A) LCG-C))
          uof (fn [st] (/ (double (inc (unsigned-bit-shift-right st 11))) DEN))
          zs (loop [st seed acc []]
               (if (>= (count acc) m)
                 acc
                 (let [s1 (step st)
                       u1 (uof s1)
                       s2 (step s1)
                       u2 (uof s2)
                       rad (Math/sqrt (* -2.0 (Math/log u1)))
                       ang (* (* 2.0 Math/PI) u2)
                       acc2 (conj acc (* rad (Math/cos ang)))]
                   (recur s2 (if (>= (count acc2) m) acc2
                                  (conj acc2 (* rad (Math/sin ang))))))))
          drift (* (- r (/ (* sg sg) 2)) t)
          vol (* sg (Math/sqrt t))
          [tot-c tot-p] (reduce (fn [[cc pp] z]
                                  (let [sp (* s0 (Math/exp (+ drift (* vol z))))
                                        sm (* s0 (Math/exp (+ drift (* vol (- z)))))]
                                    [(+ cc (max (- sp k) 0.0) (max (- sm k) 0.0))
                                     (+ pp (max (- k sp) 0.0) (max (- k sm) 0.0))]))
                                [0.0 0.0] zs)
          disc (Math/exp (* (- r) t))
          call (* disc (/ tot-c (* 2 m)))
          put (* disc (/ tot-p (* 2 m)))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "CALL " (fmt call) "\nPUT " (fmt put))))))
