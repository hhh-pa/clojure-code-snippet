;; kd_tree: exact Berg 5.2 BUILDKDTREE + SEARCHKDTREE.
;; Doubled coordinates make every region bound a closed integer bound.
(require '[clojure.string :as str])

(defn- kd-build
  "Builds a kd-tree node (nested map) over pts at depth."
  [pts depth]
  (if (= (count pts) 1)
    {:leaf true :pt (first pts)}
    (let [ax (mod depth 2)
          srt (vec (sort-by #(nth % ax) pts))
          mid (quot (dec (count srt)) 2)]
      {:leaf false :axis ax :split (nth (nth srt mid) ax)
       :left (kd-build (subvec srt 0 (inc mid)) (inc depth))
       :right (kd-build (subvec srt (inc mid)) (inc depth))})))

(defn- kd-report
  "Collects all leaf points below node."
  [node]
  (if (:leaf node)
    [(:pt node)]
    (vec (concat (kd-report (:left node)) (kd-report (:right node))))))

(defn- kd-disjoint?
  "True when the closed doubled interval [lo,hi] (nil = unbounded) misses [rl,rh]."
  [lo hi rl rh]
  (or (and (some? hi) (< hi rl)) (and (some? lo) (> lo rh))))

(defn- kd-inside?
  "True when [lo,hi] is bounded and contained in [rl,rh]."
  [lo hi rl rh]
  (and (some? lo) (some? hi) (>= lo rl) (<= hi rh)))

(defn- kd-visit
  "Reports points of node (region xlo..xhi, ylo..yhi, doubled, nil = open) in query."
  [node qxl qxr qyl qyh xlo xhi ylo yhi]
  (cond
    (or (kd-disjoint? xlo xhi qxl qxr) (kd-disjoint? ylo yhi qyl qyh))
    []
    (and (kd-inside? xlo xhi qxl qxr) (kd-inside? ylo yhi qyl qyh))
    (kd-report node)
    (:leaf node)
    (let [[x y] (:pt node)]
      (if (and (<= qxl (* 2 x) qxr) (<= qyl (* 2 y) qyh)) [[x y]] []))
    :else
    (let [s2 (* 2 (:split node))]
      (if (zero? (:axis node))
        (vec (concat (kd-visit (:left node) qxl qxr qyl qyh xlo s2 ylo yhi)
                     (kd-visit (:right node) qxl qxr qyl qyh (inc s2) xhi ylo yhi)))
        (vec (concat (kd-visit (:left node) qxl qxr qyl qyh xlo xhi ylo s2)
                     (kd-visit (:right node) qxl qxr qyl qyh xlo xhi (inc s2) yhi)))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          pts (mapv (fn [i] [(nth toks (+ 1 (* 2 i))) (nth toks (+ 2 (* 2 i)))])
                    (range n))
          qpos (+ 1 (* 2 n))
          qn (if (< qpos (count toks)) (nth toks qpos) 0)
          root (when (pos? n) (kd-build pts 0))
          lines (vec (mapcat
                      (fn [j]
                        (let [b (+ qpos 1 (* 4 j))
                              xl (nth toks b) yl (nth toks (+ b 1))
                              xr (nth toks (+ b 2)) yh (nth toks (+ b 3))
                              found (if (pos? n)
                                      (sort (kd-visit root (* 2 xl) (* 2 xr)
                                                      (* 2 yl) (* 2 yh)
                                                      nil nil nil nil))
                                      [])]
                          (if (empty? found)
                            [(str (count found))]
                            [(str (count found))
                             (str/join " " (map (fn [[x y]] (str x "," y)) found))])))
                      (range qn)))]
      (println (str/join "\n" lines)))))
