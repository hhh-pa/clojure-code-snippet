;; motion_planning: Berg 13.2 trapezoidal road map for rectangles + BFS.
(require '[clojure.string :as str])

(defn- mp-vsealed?
  [xs ys rects rays k j]
  (let [x (nth xs k)]
    (or (some (fn [[rx0 ry0 rx1 ry1]]
                (and (or (= rx0 x) (= rx1 x)) (<= ry0 (nth ys j)) (<= (nth ys (inc j)) ry1)))
              rects)
        (some (fn [[ex lo hi]]
                (and (= ex x) (<= lo (nth ys j)) (<= (nth ys (inc j)) hi)))
              rays))))

(defn- mp-hsealed?
  [xs ys rects i k]
  (let [y (nth ys k)]
    (some (fn [[rx0 ry0 rx1 ry1]]
            (and (or (= ry0 y) (= ry1 y)) (<= rx0 (nth xs i)) (<= (nth xs (inc i)) rx1)))
          rects)))

(defn- mp-open?
  [xs ys rects rays a b d]
  (cond
    (= d 0) (not (mp-vsealed? xs ys rects rays a b))
    (= d 1) (not (mp-vsealed? xs ys rects rays (inc a) b))
    (= d 2) (not (mp-hsealed? xs ys rects a b))
    :else (not (mp-hsealed? xs ys rects a (inc b)))))

(defn- mp-flood
  [nx ny blocked xs ys rects rays face id i j]
  (loop [stack [[i j]] face (assoc-in face [i j] id)]
    (if (empty? stack)
      face
      (let [[[a b] & rest] stack
            nbs (for [[da db d] [[-1 0 0] [1 0 1] [0 -1 2] [0 1 3]]
                      :let [na (+ a da) nb (+ b db)]
                      :when (and (<= 0 na (dec nx)) (<= 0 nb (dec ny))
                                 (mp-open? xs ys rects rays a b d)
                                 (not (get-in blocked [na nb]))
                                 (neg? (get-in face [na nb])))]
                  [na nb])]
        (recur (vec (concat rest nbs))
               (reduce (fn [f [na nb]] (assoc-in f [na nb] id)) face nbs))))))

(defn- mp-faces
  [nx ny blocked xs ys rects rays]
  (loop [i 0 j 0 nf 0 face (mapv (fn [_] (vec (repeat ny -1))) (range nx))]
    (cond
      (>= i nx) [face nf]
      (>= j ny) (recur (inc i) 0 nf face)
      (or (get-in blocked [i j]) (>= (get-in face [i j]) 0)) (recur i (inc j) nf face)
      :else (recur i (inc j) (inc nf) (mp-flood nx ny blocked xs ys rects rays face nf i j)))))

(defn- mp-valid?
  [[bx0 by0 bx1 by1] rects px py]
  (and (< bx0 px bx1) (< by0 py by1)
       (not (some (fn [[rx0 ry0 rx1 ry1]]
                    (and (<= rx0 px rx1) (<= ry0 py ry1)))
                  rects))))

(defn- mp-on-ray?
  [rays px py]
  (some (fn [[ex lo hi]] (and (= ex px) (< lo py hi))) rays))

(defn- mp-locate
  [xs ys face px py]
  (first (for [i (range (dec (count xs)))
               :when (<= (nth xs i) px (nth xs (inc i)))
               j (range (dec (count ys)))
               :when (and (<= (nth ys j) py (nth ys (inc j)))
                          (>= (get-in face [i j]) 0))]
           (get-in face [i j]))))

(defn- mp-bfs
  [adj fs fg]
  (loop [queue [fs] head 0 prev (assoc (vec (repeat (count adj) -1)) fs fs)]
    (cond
      (>= (nth prev fg) 0)
      (loop [cur fg path []] (if (= cur fs) (conj path cur) (recur (nth prev cur) (conj path cur))))
      (>= head (count queue)) nil
      :else
      (let [v (nth queue head)
            nbs (filter #(neg? (nth prev %)) (nth adj v))]
        (recur (vec (concat queue nbs)) (inc head)
               (reduce (fn [p w] (assoc p w v)) prev nbs))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          box [(nth toks 0) (nth toks 1) (nth toks 2) (nth toks 3)]
          [bx0 by0 bx1 by1] box
          m (nth toks 4)
          rects (mapv (fn [i] [(nth toks (+ 5 (* 4 i))) (nth toks (+ 6 (* 4 i)))
                               (nth toks (+ 7 (* 4 i))) (nth toks (+ 8 (* 4 i)))])
                      (range m))
          qpos (+ 5 (* 4 m))
          qn (if (< qpos (count toks)) (nth toks qpos) 0)
          xs (vec (sort (set (concat [bx0 bx1] (mapcat (fn [[a _ c _]] [a c]) rects)))))
          ys (vec (sort (set (concat [by0 by1] (mapcat (fn [[_ b _ d]] [b d]) rects)))))
          nx (dec (count xs)) ny (dec (count ys))
          blocked (mapv (fn [i]
                          (mapv (fn [j]
                                  (boolean
                                   (some (fn [[rx0 ry0 rx1 ry1]]
                                           (and (<= rx0 (nth xs i)) (<= (nth xs (inc i)) rx1)
                                                (<= ry0 (nth ys j)) (<= (nth ys (inc j)) ry1)))
                                         rects)))
                                (range ny)))
                        (range nx))
          rays (vec (mapcat
                     (fn [[rx0 ry0 rx1 ry1]]
                       (mapcat
                        (fn [exv]
                          (if (not (< bx0 exv bx1))
                            []
                            (let [ys2 (for [r rects :when (< (nth r 0) exv (nth r 2))
                                            y [(nth r 1) (nth r 3)]]
                                        y)
                                  ups (filter #(> % ry1) (conj (vec ys2) by1))
                                  dns (filter #(< % ry0) (conj (vec ys2) by0))]
                              (concat (when (seq ups) [[exv ry1 (apply min ups)]])
                                      (when (seq dns) [[exv (apply max dns) ry0]])))))
                        [rx0 rx1]))
                     rects))
          [face nfaces] (mp-faces nx ny blocked xs ys rects rays)
          bounds (reduce (fn [acc f]
                           (let [cells (for [i (range nx) j (range ny) :when (= (get-in face [i j]) f)]
                                             [i j])]
                             (assoc acc f [(apply min (map first cells)) (apply max (map first cells))
                                           (apply min (map second cells)) (apply max (map second cells))])))
                         {} (range nfaces))
          centers (mapv (fn [f]
                          (let [[a b c d] (bounds f)]
                            [(+ (nth xs a) (nth xs (inc b))) (+ (nth ys c) (nth ys (inc d)))]))
                        (range nfaces))
          coords (vec (concat centers (map (fn [[ex lo hi]] [(* 2 ex) (+ lo hi)]) rays)))
          adj (mapv (fn [_] []) (range (+ nfaces (count rays))))
          adj (reduce (fn [g e]
                        (let [[ex lo hi] (nth rays e)
                              k (.indexOf xs ex)
                              sides (distinct (mapcat
                                                (fn [j]
                                                  (when (and (<= lo (nth ys j)) (<= (nth ys (inc j)) hi))
                                                    [(get-in face [(dec k) j]) (get-in face [k j])]))
                                                (range ny)))
                              sides (filter #(>= % 0) sides)
                              node (+ nfaces e)]
                          (reduce (fn [h f]
                                    (-> h
                                        (update f conj node)
                                        (update node conj f)))
                                  g sides)))
                      adj (range (count rays)))
          adj (mapv (fn [lst] (vec (sort-by (fn [v] (nth coords v)) lst))) adj)
          out (mapcat
               (fn [qi]
                 (let [b (+ qpos 1 (* 4 qi))
                       sx (nth toks b) sy (nth toks (+ b 1))
                       gx (nth toks (+ b 2)) gy (nth toks (+ b 3))]
                   (if (or (not (mp-valid? box rects sx sy)) (not (mp-valid? box rects gx gy))
                           (mp-on-ray? rays sx sy) (mp-on-ray? rays gx gy))
                     ["FORBIDDEN"]
                     (let [fs (mp-locate xs ys face sx sy)
                           fg (mp-locate xs ys face gx gy)]
                       (cond
                         (or (nil? fs) (nil? fg)) ["FORBIDDEN"]
                         (= fs fg) [(str 2) (str (* 2 sx) " " (* 2 sy)) (str (* 2 gx) " " (* 2 gy))]
                         :else
                         (let [path (mp-bfs adj fs fg)]
                           (if (nil? path)
                             ["NO PATH"]
                             (let [pts (concat [[(* 2 sx) (* 2 sy)]]
                                               (map #(nth coords %) (reverse path))
                                               [[(* 2 gx) (* 2 gy)]])]
                               (concat [(str (count pts))]
                                       (map (fn [[x y]] (str x " " y)) pts))))))))))
               (range qn))]
      (println (str/join "\n" out)))))
