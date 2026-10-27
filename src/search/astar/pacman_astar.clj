;; A* pacman port of cpp/src/search/astar/pacman_astar.cc.
;; Mirrors the reference exactly: open set ordered by (cost, row, col), full
;; relaxation loop with no early exit, neighbours in up/left/right/down order,
;; and cost(dist[current] + manhattan(neighbour, food)) as in the .cc.
;; (Unreachable food exits silently; the .cc dereferences garbage parents there
;; while the Go mirror returns silently too.)
(require '[clojure.string :as str])
(let [input (slurp *in*)]
  (when-not (str/blank? input)
    (let [tok (java.util.StringTokenizer. input)
          toks (loop [v (transient [])]
                 (if (.hasMoreTokens tok)
                   (recur (conj! v (.nextToken tok)))
                   (persistent! v)))]
      (when (>= (count toks) 6)
        (let [pr (Integer/parseInt (toks 0))
              pc (Integer/parseInt (toks 1))
              fr (Integer/parseInt (toks 2))
              fc (Integer/parseInt (toks 3))
              n (Integer/parseInt (toks 4))
              m (Integer/parseInt (toks 5))
              nm (* n m)
              ^String chrs (apply str (subvec toks 6))]
          (when (and (> n 0) (> m 0) (>= (count chrs) nm))
            (let [^chars grid (char-array nm)
                  ^ints dist (int-array nm -1)
                  ^ints par (int-array nm -1)
                  ^ints costq (int-array nm 0)
                  cmp (reify java.util.Comparator
                        (compare [_ x y]
                          (let [^ints a x
                                ^ints b y
                                c (Integer/compare (aget a 0) (aget b 0))]
                            (if (zero? c)
                              (let [c2 (Integer/compare (aget a 1) (aget b 1))]
                                (if (zero? c2)
                                  (Integer/compare (aget a 2) (aget b 2))
                                  c2))
                              c))))
                  ^java.util.TreeSet q (java.util.TreeSet. ^java.util.Comparator cmp)
                  manhattan (fn [^long r ^long c]
                              (+ (Math/abs (- r fr)) (Math/abs (- c fc))))
                  pidx (+ (* pr m) pc)
                  fidx (+ (* fr m) fc)]
              (loop [i 0]
                (when (< i nm)
                  (aset grid i (.charAt chrs i))
                  (recur (inc i))))
              (aset dist pidx 0)
              (aset costq pidx (int (manhattan pr pc)))
              (.add q (int-array [(manhattan pr pc) pr pc]))
              (while (not (.isEmpty q))
                (let [^ints cur (.pollFirst q)
                      cr (aget cur 1)
                      cc (aget cur 2)
                      ci (+ (* cr m) cc)
                      dc (aget dist ci)]
                  (loop [k 0]
                    (when (< k 4)
                      (let [nr (+ cr (case k 0 -1 1 0 2 0 3 1))
                            nc (+ cc (case k 0 0 1 -1 2 1 3 0))]
                        (when (and (>= nr 0) (< nr n) (>= nc 0) (< nc m)
                                   (not= (aget grid (+ (* nr m) nc)) \%))
                          (let [ni (+ (* nr m) nc)
                                nd (aget dist ni)
                                new-dist (inc dc)
                                new-cost (+ dc (manhattan nr nc))]
                            (when (or (= nd -1) (> nd new-dist))
                              (when (not= nd -1)
                                (.remove q (int-array [(aget costq ni) nr nc])))
                              (aset dist ni new-dist)
                              (aset par ni ci)
                              (aset costq ni (int new-cost))
                              (.add q (int-array [new-cost nr nc]))))))
                      (recur (inc k))))))
              (when (not= (aget dist fidx) -1)
                (let [path (loop [s fidx acc (list)]
                             (if (= s pidx)
                               (conj acc s)
                               (recur (aget par s) (conj acc s))))
                      sb (StringBuilder.)]
                  (.append sb (str (dec (count path)) "\n"))
                  (doseq [s path]
                    (.append sb (str (quot s m) " " (rem s m) "\n")))
                  (print (str sb)))))))))))
