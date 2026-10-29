(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          genes (object-array n)]
      (dotimes [i n]
        (aset genes i (aget ^objects toks (inc i))))
      (let [health (int-array n)]
        (dotimes [i n]
          (aset health i (Integer/parseInt (aget ^objects toks (+ 1 n i)))))
        (let [cap (int 300000)
              next-node (make-array Integer/TYPE cap 26)
              fail (int-array cap)
              dict-link (int-array cap)
              gene-ids (object-array cap)
              node-count (int-array 1 1)]
          (dotimes [i cap]
            (aset gene-ids i (java.util.ArrayList.)))
          (dotimes [i n]
            (let [gene ^String (aget genes i)
                  len (.length gene)]
              (let [u (loop [ch-idx (int 0) u (int 0)]
                        (if (< ch-idx len)
                          (let [c (unchecked-subtract-int (int (.charAt gene ch-idx)) 97)]
                            (when (= (aget next-node u c) 0)
                              (let [nxt (aget node-count 0)]
                                (aset-int next-node u c nxt)
                                (aset-int node-count 0 (inc nxt))))
                            (recur (inc ch-idx) (aget next-node u c)))
                          u))]
                (.add ^java.util.ArrayList (aget gene-ids u) (int i)))))
          (let [q (java.util.ArrayDeque.)]
            (dotimes [c 26]
              (let [nxt (aget next-node 0 c)]
                (when (> nxt 0)
                  (.add q nxt))))
            (loop []
              (when-not (.isEmpty q)
                (let [u (int (.poll q))]
                  (dotimes [c 26]
                    (let [v (aget next-node u c)]
                      (when (> v 0)
                        (let [f (loop [cur (aget fail u)]
                                  (if (and (> cur 0) (= (aget next-node cur c) 0))
                                    (recur (aget fail cur))
                                    cur))
                              vf (aget next-node f c)]
                          (aset-int fail v vf)
                          (aset-int dict-link v
                                    (if (> (.size ^java.util.ArrayList (aget gene-ids vf)) 0)
                                      vf
                                      (aget dict-link vf)))
                          (.add q v)))))
                  (recur)))))
          (let [pos (+ 1 (* 2 n))
                num-strands (Integer/parseInt (aget ^objects toks pos))
                min-h (long-array 1 -1)
                max-h (long-array 1 -1)]
            (dotimes [i num-strands]
              (let [first-id (Integer/parseInt (aget ^objects toks (+ pos 1 (* i 3))))
                    last-id (Integer/parseInt (aget ^objects toks (+ pos 2 (* i 3))))
                    strand ^String (aget ^objects toks (+ pos 3 (* i 3)))
                    slen (.length strand)
                    tot (loop [si (int 0) u (int 0) acc (long 0)]
                          (if (< si slen)
                            (let [c (unchecked-subtract-int (int (.charAt strand si)) 97)
                                  nu (loop [cur u]
                                       (if (and (> cur 0) (= (aget next-node cur c) 0))
                                         (recur (aget fail cur))
                                         cur))
                                  nu (aget next-node nu c)
                                  matches (loop [curr nu add (long 0)]
                                            (if (> curr 0)
                                              (let [ids ^java.util.ArrayList (aget gene-ids curr)
                                                    sz (.size ids)
                                                    sub (loop [k (int 0) cur-add add]
                                                          (if (< k sz)
                                                            (let [gid (int (.get ids k))]
                                                              (recur (inc k)
                                                                     (if (and (>= gid first-id) (<= gid last-id))
                                                                       (+ cur-add (aget health gid))
                                                                       cur-add)))
                                                            cur-add))]
                                                (recur (aget dict-link curr) sub))
                                              add))]
                              (recur (inc si) nu (+ acc matches)))
                            acc))]
                (when (or (= (aget min-h 0) -1) (< tot (aget min-h 0)))
                  (aset-long min-h 0 tot))
                (when (or (= (aget max-h 0) -1) (> tot (aget max-h 0)))
                  (aset-long max-h 0 tot))))
            (println (str (aget min-h 0) " " (aget max-h 0)))))))))
