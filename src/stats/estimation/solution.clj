;; Standard error and bootstrap CIs (Think Stats 2e ch 8).
(require '[clojure.string :as str])
(import '[java.util Locale])

(def ^:const MOD (bit-shift-left 1 31))

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
          b (Long/parseLong (nth toks (inc n)))
          seed (Long/parseLong (nth toks (+ 2 n)))
          mean (/ (reduce + (map double xs)) n)
          variance (/ (reduce + (map (fn [x] (let [d (- (double x) mean)] (* d d))) xs)) (dec n))
          se (/ (Math/sqrt variance) (Math/sqrt n))
          lo (- mean (* 1.96 se))
          hi (+ mean (* 1.96 se))
          means (loop [k 0 rng seed acc []]
                  (if (>= k b)
                    acc
                    (let [[rng2 tot] (loop [i 0 r rng t 0]
                                       (if (>= i n)
                                         [r t]
                                         (let [r2 (mod (+ (* 1103515245 r) 12345) MOD)]
                                           (recur (inc i) r2 (+ t (nth xs (mod r2 n)))))))]
                      (recur (inc k) rng2 (conj acc (/ (double tot) n))))))
          srt (vec (sort means))
          blo (nth srt (dec (min b (max 1 (half-even (/ (* 2.5 (inc b)) 100))))))
          bhi (nth srt (dec (min b (max 1 (half-even (/ (* 97.5 (inc b)) 100))))))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "SE " (fmt se) "\nCI " (fmt lo) " " (fmt hi)
                  "\nBOOT " (fmt blo) " " (fmt bhi))))))
