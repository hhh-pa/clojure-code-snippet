
;; kalman_filter: one-dimensional Kalman filter for linear-Gaussian
;; state estimation. Predict step adds process noise; update step
;; applies the Kalman gain to each scalar measurement.
(ns kalman-filter
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn fmt6 [x]
  (String/format java.util.Locale/US "%.6f" (into-array Object [(double x)])))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [tl (int (nth nums pos))
              fr (fn [p] (/ (double (nth nums p)) (double (nth nums (inc p)))))
              q (fr (+ pos 1)) r (fr (+ pos 3))
              x0 (fr (+ pos 5)) p0 (fr (+ pos 7))
              p1 (+ pos 9)
              zs (vec (for [k (range tl)] (fr (+ p1 (* k 2)))))]
          (.append sb "KALMAN\n")
          (loop [k 0 x x0 pp p0]
            (when (< k tl)
              (let [pp2 (+ pp q)
                    kk (/ pp2 (+ pp2 r))
                    x2 (+ x (* kk (- (nth zs k) x)))
                    pp3 (* (- 1.0 kk) pp2)]
                (.append sb (str (fmt6 x2) " " (fmt6 pp3) "\n"))
                (recur (inc k) x2 pp3))))
          (recur (+ p1 (* tl 2)) (dec tcases) sb))))))

(-main)
