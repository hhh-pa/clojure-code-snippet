(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          q (Integer/parseInt (nth toks 0))
          cap (max 10 q)
          ^ints id-arr (int-array cap)
          ^longs key-arr (long-array cap)
          ^ints deg-arr (int-array cap)
          ^booleans mark-arr (boolean-array cap)
          ^ints parent-arr (int-array cap)
          ^ints child-arr (int-array cap)
          ^ints left-arr (int-array cap)
          ^ints right-arr (int-array cap)
          node-map (java.util.HashMap.)
          node-cnt (int-array 1)
          min-ref (int-array [-1])
          sb (StringBuilder.)]

      (letfn [(insert-root [idx]
                (let [min-idx (aget min-ref 0)]
                  (if (== min-idx -1)
                    (do
                      (aset left-arr idx idx)
                      (aset right-arr idx idx)
                      (aset min-ref 0 idx))
                    (let [min-left (aget left-arr min-idx)]
                      (aset right-arr idx min-idx)
                      (aset left-arr idx min-left)
                      (aset right-arr min-left idx)
                      (aset left-arr min-idx idx)
                      (when (< (aget key-arr idx) (aget key-arr min-idx))
                        (aset min-ref 0 idx))))))

              (link-roots [y x]
                (let [yl (aget left-arr y)
                      yr (aget right-arr y)]
                  (aset right-arr yl yr)
                  (aset left-arr yr yl))
                (aset parent-arr y x)
                (let [ch (aget child-arr x)]
                  (if (== ch -1)
                    (do
                      (aset child-arr x y)
                      (aset left-arr y y)
                      (aset right-arr y y))
                    (let [chl (aget left-arr ch)]
                      (aset right-arr y ch)
                      (aset left-arr y chl)
                      (aset right-arr chl y)
                      (aset left-arr ch y))))
                (aset deg-arr x (inc (aget deg-arr x)))
                (aset mark-arr y false))

              (consolidate []
                (let [max-deg 50
                      ^ints a (int-array max-deg -1)
                      min-idx (aget min-ref 0)]
                  (when (not= min-idx -1)
                    (let [root-list (java.util.ArrayList.)]
                      (loop [cur min-idx]
                        (.add root-list cur)
                        (let [nxt (aget right-arr cur)]
                          (when (not= nxt min-idx)
                            (recur nxt))))
                      (let [sz (.size root-list)]
                        (loop [i 0]
                          (when (< i sz)
                            (let [w (int (.get root-list i))]
                              (loop [x w
                                     d (aget deg-arr x)]
                                (if (and (< d max-deg) (not= (aget a d) -1))
                                  (let [y (aget a d)
                                        [nx ny] (if (> (aget key-arr x) (aget key-arr y))
                                                  [y x]
                                                  [x y])]
                                    (link-roots ny nx)
                                    (aset a d -1)
                                    (recur nx (inc d)))
                                  (when (< d max-deg)
                                    (aset a d x)))))
                            (recur (inc i)))))))
                  (aset min-ref 0 -1)
                  (loop [i 0]
                    (when (< i 50)
                      (let [nd (aget a i)]
                        (when (not= nd -1)
                          (let [cur-min (aget min-ref 0)]
                            (if (== cur-min -1)
                              (do
                                (aset min-ref 0 nd)
                                (aset left-arr nd nd)
                                (aset right-arr nd nd))
                              (let [min-left (aget left-arr cur-min)]
                                (aset right-arr nd cur-min)
                                (aset left-arr nd min-left)
                                (aset right-arr min-left nd)
                                (aset left-arr cur-min nd)
                                (when (< (aget key-arr nd) (aget key-arr cur-min))
                                  (aset min-ref 0 nd)))))))
                      (recur (inc i))))))

              (extract-min []
                (let [z (aget min-ref 0)]
                  (if (== z -1)
                    nil
                    (let [res-id (aget id-arr z)
                          res-key (aget key-arr z)
                          ch (aget child-arr z)]
                      (when (not= ch -1)
                        (let [children (java.util.ArrayList.)]
                          (loop [cur ch]
                            (.add children cur)
                            (let [nxt (aget right-arr cur)]
                              (when (not= nxt ch)
                                (recur nxt))))
                          (let [csz (.size children)
                                min-idx (aget min-ref 0)]
                            (loop [j 0]
                              (when (< j csz)
                                (let [c (int (.get children j))
                                      min-left (aget left-arr min-idx)]
                                  (aset right-arr c min-idx)
                                  (aset left-arr c min-left)
                                  (aset right-arr min-left c)
                                  (aset left-arr min-idx c)
                                  (aset parent-arr c -1))
                                (recur (inc j)))))))
                      (let [zl (aget left-arr z)
                            zr (aget right-arr z)]
                        (aset right-arr zl zr)
                        (aset left-arr zr zl)
                        (if (== z zr)
                          (aset min-ref 0 -1)
                          (do
                            (aset min-ref 0 zr)
                            (consolidate))))
                      (.remove node-map (int res-id))
                      [res-id res-key]))))

              (cut-node [x y]
                (if (== (aget right-arr x) x)
                  (aset child-arr y -1)
                  (let [xl (aget left-arr x)
                        xr (aget right-arr x)]
                    (aset right-arr xl xr)
                    (aset left-arr xr xl)
                    (when (== (aget child-arr y) x)
                      (aset child-arr y xr))))
                (aset deg-arr y (dec (aget deg-arr y)))
                (let [min-idx (aget min-ref 0)
                      min-left (aget left-arr min-idx)]
                  (aset right-arr x min-idx)
                  (aset left-arr x min-left)
                  (aset right-arr min-left x)
                  (aset left-arr min-idx x)
                  (aset parent-arr x -1)
                  (aset mark-arr x false)))

              (cascading-cut [y]
                (let [z (aget parent-arr y)]
                  (when (not= z -1)
                    (if (not (aget mark-arr y))
                      (aset mark-arr y true)
                      (do
                        (cut-node y z)
                        (cascading-cut z))))))

              (decrease-key [id new-key]
                (let [idx-obj (.get node-map (int id))]
                  (when idx-obj
                    (let [x (int idx-obj)]
                      (when (<= new-key (aget key-arr x))
                        (aset key-arr x new-key)
                        (let [y (aget parent-arr x)]
                          (when (and (not= y -1) (< (aget key-arr x) (aget key-arr y)))
                            (cut-node x y)
                            (cascading-cut y)))
                        (let [min-idx (aget min-ref 0)]
                          (when (and (not= min-idx -1) (< (aget key-arr x) (aget key-arr min-idx)))
                            (aset min-ref 0 x))))))))]

        (loop [i 0, ti 1]
          (when (and (< i q) (< ti nt))
            (let [op (nth toks ti)]
              (cond
                (= op "INSERT")
                (let [id (Integer/parseInt (nth toks (+ ti 1)))
                      val (Long/parseLong (nth toks (+ ti 2)))
                      idx (aget node-cnt 0)]
                  (aset node-cnt 0 (inc idx))
                  (aset id-arr idx id)
                  (aset key-arr idx val)
                  (aset deg-arr idx 0)
                  (aset mark-arr idx false)
                  (aset parent-arr idx -1)
                  (aset child-arr idx -1)
                  (.put node-map (int id) idx)
                  (insert-root idx)
                  (recur (inc i) (+ ti 3)))

                (= op "MIN")
                (let [min-idx (aget min-ref 0)]
                  (if (== min-idx -1)
                    (.append sb "EMPTY\n")
                    (.append sb (str (aget id-arr min-idx) " " (aget key-arr min-idx) "\n")))
                  (recur (inc i) (inc ti)))

                (= op "EXTRACT_MIN")
                (let [min-res (extract-min)]
                  (if min-res
                    (.append sb (str (first min-res) " " (second min-res) "\n"))
                    (.append sb "EMPTY\n"))
                  (recur (inc i) (inc ti)))

                (= op "DECREASE_KEY")
                (let [id (Integer/parseInt (nth toks (+ ti 1)))
                      val (Long/parseLong (nth toks (+ ti 2)))]
                  (decrease-key id val)
                  (recur (inc i) (+ ti 3)))

                :else
                (recur (inc i) (inc ti))))))
        (print (.toString sb))))))
