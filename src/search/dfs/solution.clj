;; Max 8-connected region port of cpp/src/search/dfs/solution.cc.
;; Only the region size is printed, so an iterative flood fill with the same
;; 8-neighbourhood gives identical results.
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
          len (alength all)]
      (when (>= len 2)
        (let [n (aget all 0)
              m (aget all 1)
              nm (* n m)]
          (when (>= len (+ 2 nm))
            (let [^ints g (int-array nm)]
              (loop [k 0]
                (when (< k nm)
                  (aset g k (aget all (+ 2 k)))
                  (recur (inc k))))
              (let [^java.util.ArrayDeque work (java.util.ArrayDeque.)
                    best (loop [idx 0 best -1]
                           (if (>= idx nm)
                             best
                             (if (not= (aget g idx) 1)
                               (recur (inc idx) best)
                               (do
                                 (.clear work)
                                 (.add work (int idx))
                                 (aset g idx -1)
                                 (let [sz (loop [sz 0]
                                            (if (.isEmpty work)
                                              sz
                                              (let [cur (int (.poll work))
                                                    cr (quot cur m)
                                                    cc (rem cur m)]
                                                (doseq [[dr dc] [[-1 -1] [-1 0] [-1 1]
                                                                 [0 -1] [0 1]
                                                                 [1 -1] [1 0] [1 1]]]
                                                  (let [nr (+ cr dr) nc (+ cc dc)]
                                                    (when (and (>= nr 0) (< nr n)
                                                               (>= nc 0) (< nc m))
                                                      (let [ni (+ (* nr m) nc)]
                                                        (when (= (aget g ni) 1)
                                                          (aset g ni -1)
                                                          (.add work (int ni)))))))
                                                (recur (inc sz)))))]
                                   (recur (inc idx) (max best sz)))))))]
                (print (str best "\n"))))))))))
