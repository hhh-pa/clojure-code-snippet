;; Perceptron (AIMA 19, Eq 19.8, exact rational weights).
(require '[clojure.string :as str])

(def ^:const MOD (bit-shift-left 1 31))

(defn- perc-gcd
  [a b]
  (loop [x (Math/abs a) y (Math/abs b)]
    (if (zero? y) x (recur y (mod x y)))))

(defn- perc-score
  [num x]
  (+ (nth num 0) (reduce + (map * (subvec (vec num) 1) x))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          d (nth toks 0)
          n (nth toks 1)
          u (nth toks 2)
          an (nth toks 3)
          ad (nth toks 4)
          seed (nth toks 5)
          feats (mapv (fn [i] (subvec toks (+ 6 (* i (inc d))) (+ 6 (* i (inc d)) d)))
                      (range n))
          labels (mapv #(nth toks (+ 6 (* % (inc d)) d)) (range n))]
      (loop [t 0 num (vec (repeat (inc d) 0)) rng seed mistakes 0]
        (if (>= t u)
          (let [fracs (map (fn [v] (let [g (perc-gcd v ad)]
                                     (str (quot v g) "/" (quot ad g))))
                           num)
                correct (count (filter true?
                                        (map (fn [x y]
                                               (= (if (>= (perc-score num x) 0) 1 0) y))
                                             feats labels)))]
            (print (str (str/join " " fracs) "\nACC " correct " " n
                        "\nMISTAKES " mistakes)))
          (let [rng (mod (+ (* 1103515245 rng) 12345) MOD)
                i (mod rng n)
                h (if (>= (perc-score num (nth feats i)) 0) 1 0)]
            (if (= h (nth labels i))
              (recur (inc t) num rng mistakes)
              (let [dl (* an (- (nth labels i) h))
                    x (nth feats i)
                    nnum (mapv (fn [j v]
                                 (if (zero? j) (+ v dl) (+ v (* dl (nth x (dec j))))))
                               (range (inc d)) num)]
                (recur (inc t) nnum rng (inc mistakes))))))))))
