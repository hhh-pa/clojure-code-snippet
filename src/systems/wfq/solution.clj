;; Weighted fair queueing (Tanenbaum 5.4 finish-tag formula).
(require '[clojure.string :as str])

(defn- wfq-gcd
  [a b]
  (loop [x a y b]
    (if (zero? y) x (recur y (mod x y)))))

(defn- wfq-tags
  [weights flows arrs lens]
  (let [den (reduce (fn [d w] (quot (* d w) (wfq-gcd d w))) 1 weights)]
    (loop [i 0 last (vec (repeat (count weights) 0)) tags []]
      (if (>= i (count flows))
        [den (vec tags)]
        (let [f (nth flows i)
              base (max (* (nth arrs i) den) (nth last f))
              t (+ base (* (nth lens i) (quot den (nth weights f))))]
          (recur (inc i) (assoc last f t) (conj tags t)))))))

(defn- wfq-serve
  [flows arrs lens tags]
  (let [m (count flows)]
    (loop [done (vec (repeat m false)) order [] now 0]
      (if (>= (count order) m)
        order
        (let [best (loop [i 0 b -1]
                     (if (>= i m)
                       b
                       (if (or (nth done i) (> (nth arrs i) now))
                         (recur (inc i) b)
                         (let [k [(nth tags i) (nth arrs i) (nth flows i) i]]
                           (if (or (neg? b)
                                   (neg? (compare k [(nth tags b) (nth arrs b)
                                                     (nth flows b) b])))
                             (recur (inc i) i)
                             (recur (inc i) b))))))]
          (if (neg? best)
            (recur done order
                   (reduce min (map #(nth arrs %) (remove #(nth done %) (range m)))))
            (recur (assoc done best true) (conj order best)
                   (+ now (nth lens best)))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          f (nth toks 0)
          m (nth toks 1)
          weights (subvec toks 2 (+ 2 f))
          flows (mapv #(nth toks (+ 2 f (* 3 %))) (range m))
          arrs (mapv #(nth toks (+ 3 f (* 3 %))) (range m))
          lens (mapv #(nth toks (+ 4 f (* 3 %))) (range m))
          [den tags] (wfq-tags weights flows arrs lens)
          order (wfq-serve flows arrs lens tags)
          fracs (map (fn [i]
                       (let [g (wfq-gcd (nth tags i) den)]
                         (str (quot (nth tags i) g) "/" (quot den g))))
                     order)]
      (print (str (str/join " " order) "\n" (str/join " " fracs))))))
