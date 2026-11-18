;; Permutation tests (Think Stats 2e ch 9).
(require '[clojure.string :as str])
(import '[java.util Locale])

(def ^:const MOD 2147483648)

(defn- pearson
  [xs ys]
  (let [n (count xs)
        mx (/ (reduce + xs) n)
        my (/ (reduce + ys) n)
        acc (reduce (fn [[c vx vy] [x y]]
                      (let [dx (- x mx) dy (- y my)]
                        [(+ c (* dx dy)) (+ vx (* dx dx)) (+ vy (* dy dy))]))
                    [0.0 0.0 0.0]
                    (map vector xs ys))
        [c vx vy] acc]
    (if (or (== vx 0.0) (== vy 0.0))
      0.0
      (/ c (Math/sqrt (* vx vy))))))

(defn- shuffle-fy
  [pool seed]
  (loop [i (dec (count pool)) rng seed a (vec pool)]
    (if (<= i 0)
      [a rng]
      (let [rng2 (mod (+ (* 1103515245 rng) 12345) MOD)
            j (mod rng2 (inc i))
            a2 (assoc a i (nth a j) j (nth a i))]
        (recur (dec i) rng2 a2)))))

(defn- run-diff
  [g1 g2 k seed]
  (let [n1 (count g1)
        n2 (count g2)
        s1 (reduce + g1)
        s2 (reduce + g2)
        obs (- (/ (double s1) n1) (/ (double s2) n2))
        nobs (Math/abs (- (* s1 n2) (* s2 n1)))
        pool (vec (concat g1 g2))]
    (loop [t 0 rng seed cnt 0]
      (if (>= t k)
        [obs cnt]
        (let [[a rng2] (shuffle-fy pool rng)
              t1 (reduce + (subvec a 0 n1))
              t2 (reduce + (subvec a n1))]
          (recur (inc t) rng2 (if (>= (Math/abs (- (* t1 n2) (* t2 n1))) nobs)
                                (inc cnt)
                                cnt)))))))

(defn- run-corr
  [xs ys k seed]
  (let [ysd (mapv double ys)
        obs (pearson xs ysd)
        arobs (Math/abs obs)]
    (loop [t 0 rng seed cnt 0]
      (if (>= t k)
        [obs cnt]
        (let [[a rng2] (shuffle-fy ys rng)
              r (pearson xs (mapv double a))]
          (recur (inc t) rng2 (if (>= (Math/abs r) arobs) (inc cnt) cnt)))))))

(defn- run-prop
  [g1 g2 k seed]
  (let [n1 (count g1)
        n2 (count g2)
        c1 (count (filter #(= % 1) g1))
        c2 (count (filter #(= % 1) g2))
        obs (- (/ (double c1) n1) (/ (double c2) n2))
        nobs (Math/abs (- (* c1 n2) (* c2 n1)))
        pool (vec (concat g1 g2))]
    (loop [t 0 rng seed cnt 0]
      (if (>= t k)
        [obs cnt]
        (let [[a rng2] (shuffle-fy pool rng)
              d1 (count (filter #(= % 1) (subvec a 0 n1)))
              d2 (count (filter #(= % 1) (subvec a n1)))]
          (recur (inc t) rng2 (if (>= (Math/abs (- (* d1 n2) (* d2 n1))) nobs)
                                (inc cnt)
                                cnt)))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          mode (nth toks 0)
          groups (fn []
                   (let [n1 (Long/parseLong (nth toks 1))
                         g1 (mapv #(Long/parseLong %) (subvec toks 2 (+ 2 n1)))
                         n2 (Long/parseLong (nth toks (+ 2 n1)))
                         g2 (mapv #(Long/parseLong %) (subvec toks (+ 3 n1) (+ 3 n1 n2)))
                         k (Long/parseLong (nth toks (+ 3 n1 n2)))
                         seed (Long/parseLong (nth toks (+ 4 n1 n2)))]
                     [g1 g2 k seed]))
          [obs cnt k] (cond (= mode "DIFF")
                            (let [[g1 g2 kk seed] (groups)]
                              (conj (vec (run-diff g1 g2 kk seed)) kk))
                            (= mode "CORR")
                            (let [n (Long/parseLong (nth toks 1))
                                  xs (mapv #(double (Long/parseLong (nth toks (+ 2 (* 2 %)))))
                                            (range n))
                                  ys (mapv #(Long/parseLong (nth toks (+ 3 (* 2 %)))) (range n))
                                  kk (Long/parseLong (nth toks (+ 2 (* 2 n))))
                                  seed (Long/parseLong (nth toks (+ 3 (* 2 n))))]
                              (conj (vec (run-corr xs ys kk seed)) kk))
                            :else
                            (let [[g1 g2 kk seed] (groups)]
                              (conj (vec (run-prop g1 g2 kk seed)) kk)))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "OBS " (fmt obs) "\nP " (fmt (/ (double cnt) k)))))))
