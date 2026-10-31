
;; nash_equilibrium: two-player game solving by iterated strict
;; dominance elimination, pure-strategy equilibrium enumeration and
;; 2x2 mixed-strategy solution by the indifference principle.
(ns nash-equilibrium
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn gcd* [a b]
  (let [a (if (neg? a) (- a) a) b (if (neg? b) (- b) b)]
    (loop [x a y b]
      (if (zero? y) (if (zero? x) 1 x) (recur y (rem x y))))))

(defn eliminate [r c u1 u2]
  (loop [rows (vec (repeat r true)) cols (vec (repeat c true))]
    (let [nrows (vec (for [i (range r)]
                       (if (not (nth rows i))
                         false
                         (not (some (fn [k]
                                      (and (not= k i) (nth rows k)
                                           (every? (fn [j] (or (not (nth cols j))
                                                               (> (get-in u1 [k j]) (get-in u1 [i j]))))
                                                   (range c))))
                                    (range r))))))
          rows2 nrows
          ncols (vec (for [j (range c)]
                       (if (not (nth cols j))
                         false
                         (not (some (fn [k]
                                      (and (not= k j) (nth cols k)
                                           (every? (fn [i] (or (not (nth rows2 i))
                                                               (> (get-in u2 [i k]) (get-in u2 [i j]))))
                                                   (range r))))
                                    (range c))))))]
      (if (and (= rows rows2) (= cols ncols))
        [rows2 ncols]
        (recur rows2 ncols)))))

(defn solve-one [r c u1 u2]
  (let [[rows cols] (eliminate r c u1 u2)
        rr (vec (for [i (range r) :when (nth rows i)] i))
        cc (vec (for [j (range c) :when (nth cols j)] j))
        pure (vec (for [i rr j cc
                        :when (and (every? (fn [k] (<= (get-in u1 [k j]) (get-in u1 [i j]))) rr)
                                       (every? (fn [k] (<= (get-in u2 [i k]) (get-in u2 [i j]))) cc))]
                    [i j]))
        mixed (when (and (= 2 (count rr)) (= 2 (count cc)))
                (let [[a b] rr [c0 d] cc
                      pn (- (get-in u2 [b d]) (get-in u2 [b c0]))
                      pd (- (- (get-in u2 [a c0]) (get-in u2 [b c0]))
                            (- (get-in u2 [a d]) (get-in u2 [b d])))
                      qn (- (get-in u1 [b d]) (get-in u1 [a d]))
                      qd (- (- (get-in u1 [a c0]) (get-in u1 [a d]))
                            (- (get-in u1 [b c0]) (get-in u1 [b d])))]
                  (when (and (not (zero? pd)) (not (zero? qd)))
                    (let [[pn pd] (if (neg? pd) [(- pn) (- pd)] [pn pd])
                          [qn qd] (if (neg? qd) [(- qn) (- qd)] [qn qd])
                          gp (gcd* pn pd) gq (gcd* qn qd)
                          pn (quot pn gp) pd (quot pd gp)
                          qn (quot qn gq) qd (quot qd gq)]
                      (when (and (pos? pn) (< pn pd) (pos? qn) (< qn qd))
                        [pn pd qn qd])))))]
    [rr cc pure mixed]))

(defn parse-case [nums pos]
  (let [r (int (nth nums pos)) c (int (nth nums (+ pos 1)))
        p0 (+ pos 2)
        u1 (vec (for [i (range r)] (vec (for [j (range c)] (nth nums (+ p0 (* (+ (* i c) j) 2)))))))
        u2 (vec (for [i (range r)] (vec (for [j (range c)] (nth nums (+ p0 (* (+ (* i c) j) 2) 1))))))]
    [r c u1 u2 (+ p0 (* r c 2))]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[r c u1 u2 p2] (parse-case nums pos)
              [rr cc pure mixed] (solve-one r c u1 u2)]
          (.append sb (str "NASH " (count rr) " " (count cc) "\n"))
          (.append sb (str "PURE " (count pure)
                           (if (seq pure) (str " " (str/join " " (map (fn [[i j]] (str i ":" j)) pure))) "")
                           "\n"))
          (.append sb (if mixed
                        (let [[pn pd qn qd] mixed] (str "MIXED " pn "/" pd " " qn "/" qd "\n"))
                        "MIXED NONE\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
