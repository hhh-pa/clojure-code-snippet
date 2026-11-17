;; fractional_cascading: Berg 5.6 nested-catalog chain search.
(require '[clojure.string :as str])

(defn- fc-lower
  "Lower bound of v in sorted vector a."
  [a v]
  (loop [lo 0 hi (count a)]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi) 2)]
        (if (< (nth a mid) v)
          (recur (inc mid) hi)
          (recur lo mid))))))

(defn- fc-answer
  "Answers [yl,yh] on every catalog, hopping bridges from start positions."
  [cats bridges yl yh]
  (loop [i 0 starts [] lines []]
    (if (>= i (count cats))
      lines
      (let [p (if (zero? i) (fc-lower (nth cats 0) yl) (nth (nth bridges (dec i)) (nth starts (dec i))))
            c (nth cats i)
            rep (loop [q p acc []]
                  (if (or (>= q (count c)) (> (nth c q) yh))
                    acc
                    (recur (inc q) (conj acc (nth c q)))))]
        (recur (inc i) (conj starts p)
               (conj lines (if (empty? rep)
                             "0"
                             (str (count rep) " " (str/join " " rep)))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          k (nth toks 0)
          [cats pos] (loop [i 0 p 1 acc []]
                       (if (>= i k)
                         [acc p]
                         (let [m (nth toks p)]
                           (recur (inc i) (+ p 1 m) (conj acc (subvec toks (+ p 1) (+ p 1 m)))))))
          bridges (mapv (fn [i]
                          (let [cur (nth cats i) nxt (nth cats (inc i))]
                            (conj (mapv #(fc-lower nxt %) cur) (count nxt))))
                        (range (max 0 (dec k))))
          qn (if (< pos (count toks)) (nth toks pos) 0)
          blocks (vec (mapcat
                       (fn [j]
                         (let [b (+ pos 1 (* 2 j))]
                           (when (pos? k)
                             (fc-answer cats bridges (nth toks b) (nth toks (inc b))))))
                       (range qn)))]
      (when (seq blocks)
        (println (str/join "\n" blocks))))))
