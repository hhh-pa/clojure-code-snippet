
;; forward_backward: HMM smoothing via the forward-backward algorithm.
;; Computes P(X_k | e_1:T) for every step from pi, transition, emission
;; tables and the observation sequence.
(ns forward-backward
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn fmt6 [x]
  (String/format java.util.Locale/US "%.6f" (into-array Object [(double x)])))

(defn solve-one [ns no tl pi a b obs]
  (let [al0 (vec (for [i (range ns)] (* (nth pi i) (get-in b [i (nth obs 0)]))))
        al (loop [k 1 acc [al0]]
             (if (= k tl)
               acc
               (recur (inc k) (conj acc (vec (for [j (range ns)]
                                               (* (reduce + 0.0 (for [i (range ns)]
                                                                   (* (nth (nth acc (dec k)) i)
                                                                      (get-in a [i j]))))
                                                  (get-in b [j (nth obs k)]))))))))
        be (loop [k (- tl 2) acc (assoc (vec (repeat tl nil)) (dec tl) (vec (repeat ns 1.0)))]
             (if (< k 0)
               acc
               (recur (dec k) (assoc acc k (vec (for [i (range ns)]
                                                         (reduce + 0.0 (for [j (range ns)]
                                                                         (* (get-in a [i j])
                                                                            (get-in b [j (nth obs (inc k))])
                                                                            (nth (nth acc (inc k)) j))))))))))]
    (vec (for [k (range tl)]
           (let [tot (reduce + 0.0 (for [i (range ns)] (* (nth (nth al k) i) (nth (nth be k) i))))]
             (vec (for [i (range ns)]
                    (if (zero? tot) 0.0 (/ (* (nth (nth al k) i) (nth (nth be k) i)) tot)))))))))

(defn parse-case [nums pos]
  (let [ns (int (nth nums pos)) no (int (nth nums (+ pos 1))) tl (int (nth nums (+ pos 2)))
        p0 (+ pos 3)
        fr (fn [p] (/ (double (nth nums p)) (double (nth nums (inc p)))))
        pi (vec (for [i (range ns)] (fr (+ p0 (* i 2)))))
        p1 (+ p0 (* ns 2))
        a (vec (for [i (range ns)] (vec (for [j (range ns)] (fr (+ p1 (* (+ (* i ns) j) 2)))))))
        p2 (+ p1 (* ns ns 2))
        b (vec (for [i (range ns)] (vec (for [j (range no)] (fr (+ p2 (* (+ (* i no) j) 2)))))))
        p3 (+ p2 (* ns no 2))
        obs (vec (map int (subvec nums p3 (+ p3 tl))))]
    [ns no tl pi a b obs (+ p3 tl)]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[ns no tl pi a b obs p2] (parse-case nums pos)
              gs (solve-one ns no tl pi a b obs)]
          (.append sb "FB\n")
          (doseq [row gs]
            (.append sb (str (str/join " " (map fmt6 row)) "\n")))
          (recur p2 (dec tcases) sb))))))

(-main)
