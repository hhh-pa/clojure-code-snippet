;; partition_tree: Berg 16.1 simplicial partition + SELECTINHALFPLANE.
(require '[clojure.string :as str])

(defn- pt-bisect
  "Splits idx into <=2 classes sorted by axis (ties: other axis, then index)."
  [pts idx ax]
  (if (<= (count idx) 1)
    [idx]
    (let [srt (vec (sort-by (fn [i] [(nth (nth pts i) ax) (nth (nth pts i) (- 1 ax)) i]) idx))
          mid (quot (+ (count srt) 1) 2)]
      [(subvec srt 0 mid) (subvec srt mid)])))

(defn- pt-triangle
  "Box-derived triangle strictly containing the class points."
  [pts idx]
  (let [xs (map #(nth (nth pts %) 0) idx)
        ys (map #(nth (nth pts %) 1) idx)
        xmin (apply min xs) xmax (apply max xs)
        ymin (apply min ys) ymax (apply max ys)
        w (- xmax xmin) h (- ymax ymin)]
    [xmin ymin (+ xmax h 1) ymin xmin (+ ymax w 1)]))

(defn- pt-build
  "Builds a partition-tree node (nested map) over idx."
  [pts idx]
  (if (= (count idx) 1)
    {:leaf true :pt (nth pts (first idx)) :count 1}
    (let [classes (vec (mapcat #(pt-bisect pts % 1) (pt-bisect pts idx 0)))
          kids (mapv (fn [c]
                       (let [u (pt-build pts c)]
                         [(when-not (:leaf u) (pt-triangle pts c)) u]))
                     classes)]
      {:leaf false :kids kids :count (count idx)})))

(defn- pt-eval [a b c x y]
  (+ (* a x) (* b y) c))

(defn- pt-select
  "Counts points of node in the closed half-plane a*x+b*y+c >= 0."
  [node a b c]
  (if (:leaf node)
    (let [[x y] (:pt node)]
      (if (>= (pt-eval a b c x y) 0) 1 0))
    (reduce + (map (fn [[tri u]]
                     (if (nil? tri)
                       (pt-select u a b c)
                       (let [vals (mapv (fn [k] (pt-eval a b c (nth tri (* 2 k)) (nth tri (inc (* 2 k))))) [0 1 2])]
                         (cond
                           (every? #(>= % 0) vals) (:count u)
                           (some #(>= % 0) vals) (pt-select u a b c)
                           :else 0))))
                   (:kids node)))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          pts (mapv (fn [i] [(nth toks (+ 1 (* 2 i))) (nth toks (+ 2 (* 2 i)))]) (range n))
          qpos (+ 1 (* 2 n))
          qn (if (< qpos (count toks)) (nth toks qpos) 0)
          root (when (pos? n) (pt-build pts (vec (range n))))
          lines (mapv (fn [j]
                        (let [b (+ qpos 1 (* 3 j))]
                          (str (if (pos? n)
                                 (pt-select root (nth toks b) (nth toks (+ b 1)) (nth toks (+ b 2)))
                                 0))))
                      (range qn))]
      (println (str/join "\n" lines)))))
