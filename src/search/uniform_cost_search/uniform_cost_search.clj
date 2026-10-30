
;; uniform_cost_search: UCS (Dijkstra) plus bidirectional Dijkstra on an
;; undirected weighted graph. Deterministic tie-breaks: (dist, node) queue
;; order, ascending neighbors, strict improvement, alternate sides.
(ns uniform-cost-search
  (:require [clojure.string :as str]))

(def ^:private INF 1000000000)

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn build-adj [n edges]
  (let [adj (vec (repeat n []))]
    (reduce (fn [a [u v w]]
              (-> a
                  (update u conj [v w])
                  (update v conj [u w])))
            adj edges)))

(defn ucs [adj s g]
  (let [n (count adj)
        adj (mapv #(sort-by (juxt first second) %) adj)]
    (loop [dist (assoc (vec (repeat n INF)) s 0)
           par (vec (repeat n -1))
           pq (sorted-set [0 s])]
      (if (empty? pq)
        [INF nil]
        (let [[d v] (first pq)
              pq2 (disj pq [d v])]
          (if (not= d (nth dist v))
            (recur dist par pq2)
            (if (= v g)
              [d (loop [path () x g]
                   (if (< x 0) path (recur (conj path x) (nth par x))))]
              (let [[dist2 par2 pq3]
                    (reduce (fn [[dd pp qq] [to w]]
                              (let [nd (+ d w)]
                                (if (< nd (nth dd to))
                                  [(assoc dd to nd) (assoc pp to v) (conj qq [nd to])]
                                  [dd pp qq])))
                            [dist par pq2] (nth adj v))]
                (recur dist2 par2 pq3)))))))))

(defn clean-top [pq dist]
  (loop [q pq]
    (if (empty? q)
      q
      (let [[d v] (first q)]
        (if (= d (nth dist v)) q (recur (disj q [d v])))))))

(defn bidir [adj s g]
  (let [n (count adj)
        adj (mapv #(sort-by (juxt first second) %) adj)]
    (loop [ds (assoc (vec (repeat n INF)) s 0)
           dg (assoc (vec (repeat n INF)) g 0)
           ps (vec (repeat n -1)) pg (vec (repeat n -1))
           vs (assoc (vec (repeat n false)) s true)
           vg (assoc (vec (repeat n false)) g true)
           qs (sorted-set [0 s]) qg (sorted-set [0 g])
           best INF meet -1 turn 0]
      (let [qs2 (clean-top qs ds) qg2 (clean-top qg dg)]
        (if (and (empty? qs2) (empty? qg2))
          [best meet ps pg]
          (let [ts (if (empty? qs2) INF (first (first qs2)))
                tg (if (empty? qg2) INF (first (first qg2)))]
            (if (and (< best INF) (>= (+ ts tg) best))
              [best meet ps pg]
              (if (even? turn)
                (if (empty? qs2)
                  (recur ds dg ps pg vs vg qs2 qg2 best meet (inc turn))
                  (let [[d v] (first qs2)
                        qr (disj qs2 [d v])]
                    (if (not= d (nth ds v))
                      (recur ds dg ps pg vs vg qr qg2 best meet (inc turn))
                      (let [vs2 (assoc vs v true)
                            [b0 m0] (if (and (nth vg v) (< (+ (nth ds v) (nth dg v)) best))
                                     [(+ (nth ds v) (nth dg v)) v] [best meet])
                            [ds2 ps2 vs3 q4 b1 m1]
                            (reduce (fn [[dd pp vv qq bb mm] [to w]]
                                      (let [nd (+ d w)]
                                        (if (< nd (nth dd to))
                                          (let [dd2 (assoc dd to nd) pp2 (assoc pp to v)
                                                vv2 (assoc vv to true) qq2 (conj qq [nd to])]
                                            (if (and (nth vg to) (< (+ nd (nth dg to)) bb))
                                              [dd2 pp2 vv2 qq2 (+ nd (nth dg to)) to]
                                              [dd2 pp2 vv2 qq2 bb mm]))
                                          [dd pp vv qq bb mm])))
                                    [ds ps vs2 qr b0 m0] (nth adj v))]
                        (recur ds2 dg ps2 pg vs3 vg q4 qg2 b1 m1 (inc turn))))))
                (if (empty? qg2)
                  (recur ds dg ps pg vs vg qs2 qg2 best meet (inc turn))
                  (let [[d v] (first qg2)
                        qr (disj qg2 [d v])]
                    (if (not= d (nth dg v))
                      (recur ds dg ps pg vs vg qs2 qr best meet (inc turn))
                      (let [vg2 (assoc vg v true)
                            [b0 m0] (if (and (nth vs v) (< (+ (nth ds v) (nth dg v)) best))
                                     [(+ (nth ds v) (nth dg v)) v] [best meet])
                            [dg2 pg2 vg3 q4 b1 m1]
                            (reduce (fn [[dd pp vv qq bb mm] [to w]]
                                      (let [nd (+ d w)]
                                        (if (< nd (nth dd to))
                                          (let [dd2 (assoc dd to nd) pp2 (assoc pp to v)
                                                vv2 (assoc vv to true) qq2 (conj qq [nd to])]
                                            (if (and (nth vs to) (< (+ (nth ds to) nd) bb))
                                              [dd2 pp2 vv2 qq2 (+ (nth ds to) nd) to]
                                              [dd2 pp2 vv2 qq2 bb mm]))
                                          [dd pp vv qq bb mm])))
                                    [dg pg vg2 qr b0 m0] (nth adj v))]
                        (recur ds dg2 ps pg2 vs vg3 qs2 q4 b1 m1 (inc turn))))))))))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (int (nth nums pos)) m (int (nth nums (+ pos 1)))
              s (int (nth nums (+ pos 2))) g (int (nth nums (+ pos 3)))
              p0 (+ pos 4)
              edges (vec (for [i (range m)]
                           [(int (nth nums (+ p0 (* i 3))))
                            (int (nth nums (+ p0 (* i 3) 1)))
                            (int (nth nums (+ p0 (* i 3) 2)))]))
              pos2 (+ p0 (* m 3))
              adj (build-adj n edges)
              [ud upath] (ucs adj s g)]
          (if (>= ud INF)
            (.append sb "UCS -1\n")
            (.append sb (str "UCS " ud " " (str/join " " upath) "\n")))
          (let [[best meet ps pg] (bidir adj s g)]
            (if (< meet 0)
              (.append sb "BIDIR -1\n")
              (let [left (loop [path () x meet]
                           (if (< x 0) path (recur (conj path x) (nth ps x))))
                    right (loop [acc [] x (nth pg meet)]
                            (if (< x 0) acc (recur (conj acc x) (nth pg x))))]
                (.append sb (str "BIDIR " best " " (str/join " " left)
                                 (if (seq right) (str " " (str/join " " right)) "") "\n")))))
          (recur pos2 (dec tcases) sb))))))

(-main)
