(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          m (Integer/parseInt (aget ^objects toks 1))
          adj (object-array (inc n))]
      (dotimes [i (inc n)]
        (aset ^objects adj i (java.util.ArrayList.)))
      (dotimes [i m]
        (let [u (Integer/parseInt (aget ^objects toks (+ 2 (* i 2))))
              v (Integer/parseInt (aget ^objects toks (+ 3 (* i 2))))]
          (.add ^java.util.ArrayList (aget ^objects adj u) (int v))
          (.add ^java.util.ArrayList (aget ^objects adj v) (int u))))
      (let [subtree-sz (int-array (inc n))
            cuts (int-array 1 0)]
        (letfn [(dfs [u p]
                  (let [neighbors ^java.util.ArrayList (aget ^objects adj u)
                        len (.size neighbors)
                        total (loop [i (int 0) acc (int 1)]
                                (if (< i len)
                                  (let [v (int (.get neighbors i))]
                                    (if (= v p)
                                      (recur (inc i) acc)
                                      (let [child-sz (int (dfs v u))]
                                        (when (even? child-sz)
                                          (aset cuts 0 (inc (aget cuts 0))))
                                        (recur (inc i) (+ acc child-sz)))))
                                  acc))]
                    (aset subtree-sz u total)
                    total))]
          (dfs 1 0)
          (println (aget cuts 0)))))))
