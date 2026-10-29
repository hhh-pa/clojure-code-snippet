(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (loop [p 1]
              (if (< p n)
                (recur (* p 2))
                p))]
      (letfn [(make-matrix [size]
                (vec (repeatedly size #(long-array size))))

              (add [a b]
                (let [sz (count a)
                      c (make-matrix sz)]
                  (loop [i 0]
                    (when (< i sz)
                      (let [^longs ai (nth a i)
                            ^longs bi (nth b i)
                            ^longs ci (nth c i)]
                        (loop [j 0]
                          (when (< j sz)
                            (aset ci j (+ (aget ai j) (aget bi j)))
                            (recur (inc j)))))
                      (recur (inc i))))
                  c))

              (sub [a b]
                (let [sz (count a)
                      c (make-matrix sz)]
                  (loop [i 0]
                    (when (< i sz)
                      (let [^longs ai (nth a i)
                            ^longs bi (nth b i)
                            ^longs ci (nth c i)]
                        (loop [j 0]
                          (when (< j sz)
                            (aset ci j (- (aget ai j) (aget bi j)))
                            (recur (inc j)))))
                      (recur (inc i))))
                  c))

              (standard-mult [a b]
                (let [sz (count a)
                      c (make-matrix sz)]
                  (loop [i 0]
                    (when (< i sz)
                      (let [^longs ai (nth a i)
                            ^longs ci (nth c i)]
                        (loop [k 0]
                          (when (< k sz)
                            (let [aik (aget ai k)
                                  ^longs bk (nth b k)]
                              (loop [j 0]
                                (when (< j sz)
                                  (aset ci j (+ (aget ci j) (* aik (aget bk j))))
                                  (recur (inc j)))))
                            (recur (inc k)))))
                      (recur (inc i))))
                  c))

              (get-block [src r0 c0 k]
                (let [dst (make-matrix k)]
                  (loop [i 0]
                    (when (< i k)
                      (let [^longs srow (nth src (+ i r0))
                            ^longs drow (nth dst i)]
                        (loop [j 0]
                          (when (< j k)
                            (aset drow j (aget srow (+ j c0)))
                            (recur (inc j)))))
                      (recur (inc i))))
                  dst))

              (set-block [dst r0 c0 src]
                (let [k (count src)]
                  (loop [i 0]
                    (when (< i k)
                      (let [^longs srow (nth src i)
                            ^longs drow (nth dst (+ i r0))]
                        (loop [j 0]
                          (when (< j k)
                            (aset drow (+ j c0) (aget srow j))
                            (recur (inc j)))))
                      (recur (inc i))))
                  dst))

              (strassen [a b]
                (let [sz (count a)]
                  (if (<= sz 2)
                    (standard-mult a b)
                    (let [k (quot sz 2)
                          a11 (get-block a 0 0 k)
                          a12 (get-block a 0 k k)
                          a21 (get-block a k 0 k)
                          a22 (get-block a k k k)
                          b11 (get-block b 0 0 k)
                          b12 (get-block b 0 k k)
                          b21 (get-block b k 0 k)
                          b22 (get-block b k k k)
                          m1 (strassen (add a11 a22) (add b11 b22))
                          m2 (strassen (add a21 a22) b11)
                          m3 (strassen a11 (sub b12 b22))
                          m4 (strassen a22 (sub b21 b11))
                          m5 (strassen (add a11 a12) b22)
                          m6 (strassen (sub a21 a11) (add b11 b12))
                          m7 (strassen (sub a12 a22) (add b21 b22))
                          c11 (add (sub (add m1 m4) m5) m7)
                          c12 (add m3 m5)
                          c21 (add m2 m4)
                          c22 (add (add (sub m1 m2) m3) m6)
                          c (make-matrix sz)]
                      (set-block c 0 0 c11)
                      (set-block c 0 k c12)
                      (set-block c k 0 c21)
                      (set-block c k k c22)
                      c))))]

        (let [a (make-matrix m)
              b (make-matrix m)]
          (loop [i 0, ti 1]
            (when (< i n)
              (let [^longs row (nth a i)]
                (loop [j 0, tj ti]
                  (when (< j n)
                    (aset row j (Long/parseLong (nth toks tj)))
                    (recur (inc j) (inc tj)))))
              (recur (inc i) (+ ti n))))
          (loop [i 0, ti (+ 1 (* n n))]
            (when (< i n)
              (let [^longs row (nth b i)]
                (loop [j 0, tj ti]
                  (when (< j n)
                    (aset row j (Long/parseLong (nth toks tj)))
                    (recur (inc j) (inc tj)))))
              (recur (inc i) (+ ti n))))
          (let [c (strassen a b)]
            (loop [i 0]
              (when (< i n)
                (let [^longs row (nth c i)]
                  (println (clojure.string/join " " (loop [j 0, acc []]
                                                              (if (< j n)
                                                                (recur (inc j) (conj acc (aget row j)))
                                                                acc)))))
                (recur (inc i))))))))))
