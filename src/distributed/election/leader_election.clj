(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          ^ints ring (int-array n)
          leader-ref (int-array [-1])]
      (loop [i 0]
        (when (< i n)
          (let [v (Integer/parseInt (nth toks (inc i)))]
            (aset ring i v)
            (when (> v (aget leader-ref 0))
              (aset leader-ref 0 v)))
          (recur (inc i))))

      (letfn [(simulate-lcr []
                (loop [i 0, msgs 0]
                  (if (< i n)
                    (let [uid (aget ring i)
                          inc-msgs (loop [cur i, m 0]
                                     (let [m1 (inc m)
                                           nxt (rem (inc cur) n)]
                                       (if (or (== nxt i) (> (aget ring nxt) uid))
                                         m1
                                         (recur nxt m1))))]
                      (recur (inc i) (+ msgs inc-msgs)))
                    msgs)))

              (simulate-hs []
                (loop [phase 0, msgs 0]
                  (let [limit (bit-shift-left 1 phase)
                        [new-msgs leader-found?]
                        (loop [i 0, cur-msgs msgs]
                          (if (< i n)
                            (let [uid (aget ring i)
                                  res-i (loop [dirs (seq [-1 1]), m cur-msgs]
                                          (if dirs
                                            (let [dir (first dirs)
                                                  sub (loop [dist 0, cur i, m-sub m]
                                                        (if (< dist limit)
                                                          (let [next-cur (rem (+ (+ cur dir) n) n)
                                                                next-m (inc m-sub)
                                                                next-dist (inc dist)]
                                                            (if (== next-cur i)
                                                              [next-m true true next-dist]
                                                              (if (> (aget ring next-cur) uid)
                                                                [next-m false false next-dist]
                                                                (recur next-dist next-cur next-m))))
                                                          [m-sub true false dist]))
                                                  [m-sub ok? found? d-traversed] sub]
                                              (if found?
                                                [m-sub true]
                                                (let [ret-m (if ok? (+ m-sub d-traversed) m-sub)]
                                                  (recur (next dirs) ret-m))))
                                            [m false]))
                                  [m-i found?] res-i]
                              (if found?
                                [m-i true]
                                (recur (inc i) m-i)))
                            [cur-msgs false]))]
                    (if (or leader-found? (>= limit (quot n 2)))
                      new-msgs
                      (recur (inc phase) new-msgs)))))]

        (let [lcr-msgs (simulate-lcr)
              hs-msgs (simulate-hs)]
          (println (str "LEADER: " (aget leader-ref 0)))
          (println (str "LCR MESSAGES: " lcr-msgs))
          (println (str "HS MESSAGES: " hs-msgs)))))))
