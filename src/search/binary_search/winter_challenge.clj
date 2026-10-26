;; Exact port of cpp/src/search/binary_search/winter_challenge.cc, including its
;; hand-rolled lower/upper bound semantics (loop-exit returns last mid).
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
      (when (pos? len)
        (let [n (aget all 0)]
          (if (or (<= n 0) (> (+ 2 n) len))
            (print "-1 -1 0\n")
            (letfn [(lower-bound [^long N ^long K]
                      (loop [low (long 0) high (dec N) mid (long 0)]
                        (if (> low high)
                          mid
                          (let [mid2 (quot (+ low high) 2)]
                            (cond
                              (and (>= (aget all (+ 1 mid2)) K)
                                   (or (zero? mid2) (< (aget all mid2) K)))
                              mid2
                              (>= (aget all (+ 1 mid2)) K)
                              (recur low (dec mid2) mid2)
                              :else
                              (recur (inc mid2) high mid2))))))
                    (upper-bound [^long N ^long K]
                      (loop [low (long 0) high (dec N) mid (long 0)]
                        (if (> low high)
                          mid
                          (let [mid2 (quot (+ low high) 2)]
                            (cond
                              (and (> (aget all (+ 1 mid2)) K)
                                   (or (zero? mid2) (<= (aget all mid2) K)))
                              mid2
                              (> (aget all (+ 1 mid2)) K)
                              (recur low (dec mid2) mid2)
                              :else
                              (recur (inc mid2) high mid2))))))]
              (let [K (aget all (+ 1 n))
                    li (lower-bound n K)
                    ui (upper-bound n K)]
                (if (not= (aget all (+ 1 li)) K)
                  (print "-1 -1 0\n")
                  (print (str li " " (dec ui) " " (- ui li) "\n")))))))))))
