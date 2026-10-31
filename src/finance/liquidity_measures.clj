;; liquidity_measures: Roll's spread, Corwin-Schultz high-low spread,
;; Kyle's lambda, Amihud's illiquidity and VPIN (volume-synchronized
;; probability of informed trading) over min(50,B) volume buckets.
;; Batch 6 (T376). Input: B, OHLCV bars.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [B (Long/parseLong (nth toks 0))
            gv (fn [k f] (mapv (fn [i] (Double/parseDouble (nth toks (+ 1 (+ (* i 5) f))))) (range k)))
            H (gv B 1) L (gv B 2) C (gv B 3) V (gv B 4)
            roll (if (< B 3) 0.0
                   (let [n (- B 2)
                         m1 (/ (reduce + (map (fn [t] (- (nth C t) (nth C (dec t)))) (range 1 (dec B)))) n)
                         m2 (/ (reduce + (map (fn [t] (- (nth C (inc t)) (nth C t))) (range 1 (dec B)))) n)
                         cv (/ (reduce + (map (fn [t] (* (- (- (nth C t) (nth C (dec t))) m1)
                                                        (- (- (nth C (inc t)) (nth C t)) m2))) (range 1 (dec B)))) n)]
                     (if (< cv 0) (* 2 (Math/sqrt (- cv))) 0.0)))
            cs (if (< B 2) 0.0
                 (let [ss (map (fn [t]
                                  (let [b0 (Math/log (/ (nth H t) (nth L t)))
                                        b1 (Math/log (/ (nth H (inc t)) (nth L (inc t))))
                                        beta (+ (* b0 b0) (* b1 b1))
                                        g (Math/log (/ (max (nth H t) (nth H (inc t))) (min (nth L t) (nth L (inc t)))))
                                        gamma (* g g)
                                        alpha (- (/ (- (Math/sqrt (* 2 beta)) (Math/sqrt beta)) (- 3 (* 2 (Math/sqrt 2.0))))
                                                 (Math/sqrt (/ gamma (- 3 (* 2 (Math/sqrt 2.0))))))
                                        st (/ (* 2 (- (Math/exp alpha) 1)) (+ 1 (Math/exp alpha)))]
                                    (max 0.0 st))) (range (dec B)))]
                   (/ (reduce + ss) (count ss))))
            sg (loop [t 1 s (assoc (vec (repeat B 1)) 0 1)]
                 (if (>= t B) s
                   (recur (inc t) (assoc s t (cond (> (nth C t) (nth C (dec t))) 1 (< (nth C t) (nth C (dec t))) -1 :else (nth s (dec t)))))))
            kyle (if (< B 2) 0.0
                   (let [n (dec B)
                         mx (/ (reduce + (map (fn [t] (* (nth sg t) (nth V t))) (range 1 B))) n)
                         mr (/ (reduce + (map (fn [t] (dec (/ (nth C t) (nth C (dec t))))) (range 1 B))) n)
                         cv (reduce + (map (fn [t] (* (- (* (nth sg t) (nth V t)) mx)
                                                        (- (dec (/ (nth C t) (nth C (dec t)))) mr))) (range 1 B)))
                         vv (reduce + (map (fn [t] (let [d (- (* (nth sg t) (nth V t)) mx)] (* d d))) (range 1 B)))]
                     (if (pos? vv) (/ cv vv) 0.0)))
            amihud (let [terms (filter some?
                                      (map (fn [t] (let [dv (* (nth C t) (nth V t))]
                                                     (when (pos? dv) (/ (Math/abs (dec (/ (nth C t) (nth C (dec t))))) dv))))
                                           (range 1 B)))]
                     (if (seq terms) (/ (reduce + terms) (count terms)) 0.0))
            tot (reduce + V)
            nb (max 1 (min 50 B))
            vb (/ tot nb)
            vpin (let [step (fn step [t fill buy sell acc]
                                (if (>= t B)
                                  (if (> fill 1e-12) (+ acc (Math/abs (- buy sell))) acc)
                                  (let [[f b s a] (loop [rem (nth V t) f fill b buy s sell a acc]
                                                    (if (<= rem 1e-12) [f b s a]
                                                      (let [take (min rem (- vb f))
                                                            b2 (if (pos? (nth sg t)) (+ b take) b)
                                                            s2 (if (pos? (nth sg t)) s (+ s take))
                                                            f2 (+ f take)
                                                            r2 (- rem take)]
                                                        (if (>= f2 (- vb 1e-12))
                                                          (recur r2 0.0 0.0 0.0 (+ a (Math/abs (- b2 s2))))
                                                          (recur r2 f2 b2 s2 a)))))]
                                    (step (inc t) f b s a))))]
                       (step 0 0.0 0.0 0.0 0.0))
            vpinv (if (pos? tot) (/ vpin tot) 0.0)]
        (println (str "ROLL: " (fmt "%.6f" roll)))
        (println (str "CS: " (fmt "%.6f" cs)))
        (println (str "KYLE: " (fmt "%.6f" kyle)))
        (println (str "AMIHUD: " (fmt "%.6f" amihud)))
        (println (str "VPIN: " (fmt "%.6f" vpinv)))))))
(-main (slurp *in*))
