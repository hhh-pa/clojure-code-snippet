;; bond_duration: bond price, Macaulay/modified duration, convexity
;; (annualized, semiannual compounding), key-rate durations (+/-1bp per key
;; tenor) and Redington immunization check vs a single liability payment.
;; Batch 6 (T380). Input: ytm m, (period cf)*, keys, liability (period amt).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [ytm (Double/parseDouble (nth toks 0)) m (Long/parseLong (nth toks 1))
            kp (mapv (fn [i] (Long/parseLong (nth toks (+ 2 (* 2 i))))) (range m))
            cf (mapv (fn [i] (Double/parseDouble (nth toks (+ 3 (* 2 i))))) (range m))
            yp (/ ytm 2.0)
            df (fn [k] (Math/pow (+ 1.0 yp) (- k)))
            P (reduce + (map (fn [i] (* (nth cf i) (df (nth kp i)))) (range m)))
            dper (/ (reduce + (map (fn [i] (* (nth kp i) (nth cf i) (df (nth kp i)))) (range m))) P)
            cper (/ (reduce + (map (fn [i] (* (nth kp i) (inc (nth kp i)) (nth cf i) (df (nth kp i)))) (range m)))
                      (* P (+ 1.0 yp) (+ 1.0 yp)))
            mac (/ dper 2.0) mod (/ mac (+ 1.0 yp)) conv (/ cper 4.0)
            q (+ 2 (* 2 m))
            nk (Long/parseLong (nth toks q))
            keys (mapv (fn [i] (Long/parseLong (nth toks (+ q 1 i)))) (range nk))
            krd (mapv (fn [j]
                        (let [key (nth keys j)
                              pu (reduce + (map (fn [i] (let [r (if (= (nth kp i) key) (+ yp 0.0001) yp)]
                                                                  (* (nth cf i) (Math/pow (+ 1.0 r) (- (nth kp i)))))) (range m)))
                              pd (reduce + (map (fn [i] (let [r (if (= (nth kp i) key) (- yp 0.0001) yp)]
                                                                  (* (nth cf i) (Math/pow (+ 1.0 r) (- (nth kp i)))))) (range m)))]
                          (/ (/ (- (- pu pd)) (* 2.0 P 0.0001)) 2.0))) (range nk))
            q2 (+ q 1 nk)
            kl (Long/parseLong (nth toks q2)) al (Double/parseDouble (nth toks (+ q2 1)))
            pl (* al (Math/pow (+ 1.0 yp) (- kl)))
            dl (double kl) cl (/ (* kl (inc kl)) (* (+ 1.0 yp) (+ 1.0 yp)))
            imm (and (< (/ (Math/abs (- P pl)) pl) 1e-6) (< (Math/abs (- dper dl)) 1e-4) (> cper (+ cl 1e-9)))]
        (println (str "PRICE: " (fmt "%.6f" P)))
        (println (str "MAC: " (fmt "%.6f" mac)))
        (println (str "MOD: " (fmt "%.6f" mod)))
        (println (str "CONV: " (fmt "%.6f" conv)))
        (println (str "KRD:" (apply str (map (fn [x] (str " " (fmt "%.6f" x))) krd))))
        (println (str "IMMUNIZED: " (if imm "YES" "NO")))))))
(-main (slurp *in*))
