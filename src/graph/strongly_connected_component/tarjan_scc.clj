(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          ^ints head (int-array n -1)
          ^ints edge-to (int-array (max 1 m))
          ^ints edge-next (int-array (max 1 m) -1)
          edge-cnt (int-array 1)
          ^ints dfn (int-array n)
          ^ints low (int-array n)
          ^booleans on-stack (boolean-array n false)
          ^ints st (int-array (max 1 n))
          st-sz (int-array 1)
          timer (int-array 1)
          sccs (atom [])]
      (letfn [(dfs [u]
                (let [t (inc (aget timer 0))]
                  (aset timer 0 t)
                  (aset dfn u t)
                  (aset low u t))
                (let [sz (aget st-sz 0)]
                  (aset st sz u)
                  (aset st-sz 0 (inc sz)))
                (aset on-stack u true)
                (loop [e (aget head u)]
                  (when (not= e -1)
                    (let [v (aget edge-to e)]
                      (if (== (aget dfn v) 0)
                        (do
                          (dfs v)
                          (aset low u (min (aget low u) (aget low v))))
                        (when (aget on-stack v)
                          (aset low u (min (aget low u) (aget dfn v))))))
                    (recur (aget edge-next e))))
                (when (== (aget low u) (aget dfn u))
                  (let [scc (loop [acc []]
                              (let [sz (dec (aget st-sz 0))
                                    node (aget st sz)]
                                (aset st-sz 0 sz)
                                (aset on-stack node false)
                                (let [nacc (conj acc node)]
                                  (if (== node u)
                                    nacc
                                    (recur nacc)))))]
                    (swap! sccs conj (vec (sort scc))))))]
        (loop [i 0, ti 2]
          (when (< i m)
            (let [u (Integer/parseInt (nth toks ti))
                  v (Integer/parseInt (nth toks (+ ti 1)))
                  e (aget edge-cnt 0)]
              (aset edge-cnt 0 (inc e))
              (aset edge-to e v)
              (aset edge-next e (aget head u))
              (aset head u e)
              (recur (inc i) (+ ti 2)))))
        (loop [i 0]
          (when (< i n)
            (when (== (aget dfn i) 0)
              (dfs i))
            (recur (inc i))))
        (let [sorted (sort-by first @sccs)]
          (println (count sorted))
          (doseq [scc sorted]
            (println (clojure.string/join " " scc))))))))
