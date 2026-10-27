;; BFS shortest reach port of cpp/src/search/bfs/ctci_bfs_shortest_reach.cc
;; (the Go mirror implements identical logic).
(require '[clojure.string :as str])
(let [input (slurp *in*)]
  (when-not (str/blank? input)
    (let [tok (java.util.StringTokenizer. input)
          ^ints all (let [a (int-array (.countTokens tok))]
                      (loop [i 0]
                        (if (.hasMoreTokens tok)
                          (do (aset a i (Integer/parseInt (.nextToken tok)))
                              (recur (inc i)))
                          a)))
          len (alength all)
          sb (StringBuilder.)]
      (when (pos? len)
        (loop [p 1 qq (aget all 0)]
          (when (and (pos? qq) (< (inc p) len))
            (let [n (aget all p)
                  m (aget all (inc p))
                  adj (vec (repeatedly n #(java.util.ArrayList.)))
                  pp (loop [i 0 pp (+ p 2)]
                       (if (>= i m)
                         pp
                         (let [u (dec (aget all pp))
                               v (dec (aget all (inc pp)))
                               ^java.util.ArrayList au (nth adj u)
                               ^java.util.ArrayList av (nth adj v)]
                           (.add au (int v))
                           (.add av (int u))
                           (recur (inc i) (+ pp 2)))))
                  start (dec (aget all pp))
                  ^ints dist (int-array n -1)
                  ^java.util.ArrayDeque q (java.util.ArrayDeque.)]
              (aset dist start 0)
              (.add q (int start))
              (while (not (.isEmpty q))
                (let [cur (int (.poll q))
                      dc (aget dist cur)
                      ^java.util.ArrayList lst (nth adj cur)
                      sz (.size lst)]
                  (loop [j 0]
                    (when (< j sz)
                      (let [nxt (int (.get lst j))
                            nd (+ dc 6)]
                        (when (or (= (aget dist nxt) -1) (< nd (aget dist nxt)))
                          (aset dist nxt nd)
                          (.add q (int nxt))))
                      (recur (inc j))))))
              (loop [i 0]
                (when (< i n)
                  (when (not= i start)
                    (.append sb (str (aget dist i)))
                    (when (not= i (dec n))
                      (.append sb " ")))
                  (recur (inc i))))
              (.append sb "\n")
              (recur (long (inc pp)) (dec qq))))))
      (print (str sb)))))
