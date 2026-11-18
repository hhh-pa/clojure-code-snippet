;; PMF/CDF queries (Think Stats 2e ch 3-4).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- half-even
  [r]
  (let [q (long (Math/floor r))
        f (- r q)]
    (cond (> f 0.5) (inc q)
          (< f 0.5) q
          (even? q) q
          :else (inc q))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          xs (mapv #(Long/parseLong %) (subvec toks 1 (inc n)))
          srt (vec (sort xs))
          i0 (inc n)
          q (Long/parseLong (nth toks i0))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          out (loop [k 0 i (inc i0) acc []]
                (if (>= k q)
                  acc
                  (let [op (nth toks i)]
                    (if (= op "PCTL")
                      (let [p (Double/parseDouble (nth toks (inc i)))
                            r (/ (* p (inc n)) 100)
                            rank (min n (max 1 (half-even r)))]
                        (recur (inc k) (+ i 2) (conj acc (str (nth srt (dec rank))))))
                      (let [v (Long/parseLong (nth toks (inc i)))
                            cnt (count (filter (if (= op "PMF")
                                                 #(= % v)
                                                 #(<= % v))
                                               xs))]
                        (recur (inc k) (+ i 2)
                               (conj acc (if (or (= op "PMF") (= op "CDF"))
                                           (fmt (/ (double cnt) n))
                                           (fmt (/ (double (* 100 cnt)) n))))))))))]
      (print (str/join "\n" out)))))
