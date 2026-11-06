;; link-state-routing: OSPF flooding + per-router Dijkstra SPT.
;; Input: N E; E undirected links. Each router floods and runs
;; Dijkstra locally. Output N lines; line i: per dest "dist:nh"
;; ("0:-" self, "INF:-" unreachable). Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-int [tok] (parse-token tok))

(def INFLL 4000000000000000000)

(defn dijkstra [cost N s]
  (loop [it 0 dist (assoc (vec (repeat N INFLL)) s 0)
         parent (vec (repeat N -1)) done (vec (repeat N false))]
    (if (>= it N) [dist parent]
      (let [u (loop [i 0 best -1]
                (if (>= i N) best
                  (recur (+ i 1) (if (and (not (nth done i))
                                          (or (= best -1) (< (nth dist i) (nth dist best)))) i best))))]
        (if (or (= u -1) (= (nth dist u) INFLL)) [dist parent]
          (let [[dist2 parent2] (loop [v 0 dd dist pp parent]
                                  (if (>= v N) [dd pp]
                                    (if (and (< (nth (nth cost u) v) (quot INFLL 2))
                                             (< (+ (nth dd u) (nth (nth cost u) v)) (nth dd v)))
                                      (recur (+ v 1) (assoc dd v (+ (nth dd u) (nth (nth cost u) v))) (assoc pp v u))
                                      (recur (+ v 1) dd pp))))]
            (recur (+ it 1) dist2 parent2 (assoc done u true))))))))

(defn solve [toks]
  (let [N (read-int (nth toks 0)) E (read-int (nth toks 1))
        cost (loop [i 0 c (vec (repeat N (vec (repeat N INFLL))))]
               (if (>= i E) c
                 (let [u (read-int (nth toks (+ 2 (* i 3))))
                       v (read-int (nth toks (+ 3 (* i 3))))
                       cc (read-int (nth toks (+ 4 (* i 3))))]
                   (recur (+ i 1) (assoc c u (assoc (nth c u) v cc) v (assoc (nth c v) u cc))))))]
    (mapv (fn [s] (let [[dist parent] (dijkstra cost N s)]
                    (apply str (interpose " " (mapv (fn [d]
                                                       (cond (= d s) "0:-"
                                                             (>= (nth dist d) (quot INFLL 2)) "INF:-"
                                                             :else (let [x (loop [x d] (if (= (nth parent x) s) x (recur (nth parent x))))]
                                                                     (str (nth dist d) ":" x)))) (range N))))))
          (range N))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
