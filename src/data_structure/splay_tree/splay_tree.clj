(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          q (Integer/parseInt (nth toks 0))
          cap (max 20 (+ q 5))
          ^ints key-arr (int-array cap)
          ^ints left-arr (int-array cap -1)
          ^ints right-arr (int-array cap -1)
          ^ints parent-arr (int-array cap -1)
          root-ref (int-array [-1])
          node-cnt (int-array [0])
          sb (StringBuilder.)]

      (letfn [(rotate-left [x]
                (let [y (aget right-arr x)]
                  (when (not= y -1)
                    (let [yl (aget left-arr y)]
                      (aset right-arr x yl)
                      (when (not= yl -1)
                        (aset parent-arr yl x)))
                    (let [p (aget parent-arr x)]
                      (aset parent-arr y p)
                      (cond
                        (== p -1) (aset root-ref 0 y)
                        (== (aget left-arr p) x) (aset left-arr p y)
                        :else (aset right-arr p y)))
                    (aset left-arr y x)
                    (aset parent-arr x y))))

              (rotate-right [x]
                (let [y (aget left-arr x)]
                  (when (not= y -1)
                    (let [yr (aget right-arr y)]
                      (aset left-arr x yr)
                      (when (not= yr -1)
                        (aset parent-arr yr x)))
                    (let [p (aget parent-arr x)]
                      (aset parent-arr y p)
                      (cond
                        (== p -1) (aset root-ref 0 y)
                        (== (aget left-arr p) x) (aset left-arr p y)
                        :else (aset right-arr p y)))
                    (aset right-arr y x)
                    (aset parent-arr x y))))

              (splay [x]
                (loop []
                  (let [p (aget parent-arr x)]
                    (when (not= p -1)
                      (let [g (aget parent-arr p)]
                        (if (== g -1)
                          (if (== (aget left-arr p) x)
                            (rotate-right p)
                            (rotate-left p))
                          (let [x-left? (== (aget left-arr p) x)
                                p-left? (== (aget left-arr g) p)]
                            (cond
                              (and x-left? p-left?)
                              (do (rotate-right g) (rotate-right p))
                              (and (not x-left?) (not p-left?))
                              (do (rotate-left g) (rotate-left p))
                              (and (not x-left?) p-left?)
                              (do (rotate-left p) (rotate-right g))
                              :else
                              (do (rotate-right p) (rotate-left g))))))
                      (recur)))))

              (insert [k]
                (let [r (aget root-ref 0)]
                  (if (== r -1)
                    (let [idx (aget node-cnt 0)]
                      (aset node-cnt 0 (inc idx))
                      (aset key-arr idx k)
                      (aset left-arr idx -1)
                      (aset right-arr idx -1)
                      (aset parent-arr idx -1)
                      (aset root-ref 0 idx))
                    (let [[p is-left? exists?]
                          (loop [cur r, p -1]
                            (if (== cur -1)
                              [p false false]
                              (let [ck (aget key-arr cur)]
                                (cond
                                  (< k ck)
                                  (let [nxt (aget left-arr cur)]
                                    (if (== nxt -1)
                                      [cur true false]
                                      (recur nxt cur)))
                                  (> k ck)
                                  (let [nxt (aget right-arr cur)]
                                    (if (== nxt -1)
                                      [cur false false]
                                      (recur nxt cur)))
                                  :else
                                  [cur false true]))))]
                      (if exists?
                        (splay p)
                        (let [idx (aget node-cnt 0)]
                          (aset node-cnt 0 (inc idx))
                          (aset key-arr idx k)
                          (aset left-arr idx -1)
                          (aset right-arr idx -1)
                          (aset parent-arr idx p)
                          (if is-left?
                            (aset left-arr p idx)
                            (aset right-arr p idx))
                          (splay idx)))))))

              (find-key [k]
                (let [r (aget root-ref 0)]
                  (if (== r -1)
                    false
                    (let [[found-node last-node]
                          (loop [cur r, last r]
                            (if (== cur -1)
                              [-1 last]
                              (let [ck (aget key-arr cur)]
                                (cond
                                  (< k ck) (recur (aget left-arr cur) cur)
                                  (> k ck) (recur (aget right-arr cur) cur)
                                  :else [cur cur]))))]
                      (if (not= found-node -1)
                        (do (splay found-node) true)
                        (do (splay last-node) false))))))

              (remove-key [k]
                (when (find-key k)
                  (let [r (aget root-ref 0)
                        l-sub (aget left-arr r)
                        r-sub (aget right-arr r)]
                    (when (not= l-sub -1) (aset parent-arr l-sub -1))
                    (when (not= r-sub -1) (aset parent-arr r-sub -1))
                    (if (== l-sub -1)
                      (aset root-ref 0 r-sub)
                      (let [m (loop [cur l-sub]
                                (let [nxt (aget right-arr cur)]
                                  (if (== nxt -1) cur (recur nxt))))]
                        (aset root-ref 0 l-sub)
                        (splay m)
                        (aset right-arr m r-sub)
                        (when (not= r-sub -1)
                          (aset parent-arr r-sub m)))))))

              (inorder [u first-box]
                (when (not= u -1)
                  (inorder (aget left-arr u) first-box)
                  (if (aget ^booleans first-box 0)
                    (do
                      (.append sb (str (aget key-arr u)))
                      (aset ^booleans first-box 0 false))
                    (.append sb (str " " (aget key-arr u))))
                  (inorder (aget right-arr u) first-box)))]

        (loop [i 0, ti 1]
          (when (and (< i q) (< ti nt))
            (let [op (nth toks ti)]
              (cond
                (= op "INSERT")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (insert val)
                  (recur (inc i) (+ ti 2)))

                (= op "FIND")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (.append sb (if (find-key val) "FOUND\n" "NOT_FOUND\n"))
                  (recur (inc i) (+ ti 2)))

                (= op "DELETE")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (remove-key val)
                  (recur (inc i) (+ ti 2)))

                (= op "PRINT")
                (let [first-box (boolean-array [true])]
                  (inorder (aget root-ref 0) first-box)
                  (.append sb "\n")
                  (recur (inc i) (inc ti)))

                :else
                (recur (inc i) (inc ti))))))
        (print (.toString sb))))))
