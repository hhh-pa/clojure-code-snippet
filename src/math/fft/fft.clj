(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          mod 998244353
          g 3
          deg-a (Integer/parseInt (nth toks 0))
          deg-b (Integer/parseInt (nth toks 1))
          n (loop [sz 1]
              (if (< sz (+ deg-a deg-b 1))
                (recur (* sz 2))
                sz))
          ^longs a (long-array n)
          ^longs b (long-array n)]
      (letfn [(mod-pow [base exp]
                (loop [b (rem base mod)
                       e exp
                       res 1]
                  (if (> e 0)
                    (recur (rem (* b b) mod)
                           (quot e 2)
                           (if (== (rem e 2) 1)
                             (rem (* res b) mod)
                             res))
                    res)))
              (mod-inv [x]
                (mod-pow x (- mod 2)))
              (ntt [^longs arr invert]
                (loop [i 1
                       j 0]
                  (when (< i n)
                    (let [nj (loop [bit (bit-shift-right n 1)
                                    jj j]
                               (if (not= (bit-and jj bit) 0)
                                 (recur (bit-shift-right bit 1) (bit-xor jj bit))
                                 (bit-xor jj bit)))]
                      (when (< i nj)
                        (let [tmp (aget arr i)]
                          (aset arr i (aget arr nj))
                          (aset arr nj tmp)))
                      (recur (inc i) nj))))
                (loop [len 2]
                  (when (<= len n)
                    (let [wlen (let [w (mod-pow g (quot (- mod 1) len))]
                                 (if invert (mod-inv w) w))
                          half (quot len 2)]
                      (loop [i 0]
                        (when (< i n)
                          (loop [j 0
                                 w 1]
                            (when (< j half)
                              (let [u (aget arr (+ i j))
                                    v (rem (* (aget arr (+ i j half)) w) mod)
                                    sum (+ u v)
                                    diff (- u v)]
                                (aset arr (+ i j) (if (< sum mod) sum (- sum mod)))
                                (aset arr (+ i j half) (if (>= diff 0) diff (+ diff mod)))
                                (recur (inc j) (rem (* w wlen) mod)))))
                          (recur (+ i len)))))
                    (recur (* len 2))))
                (when invert
                  (let [n-inv (mod-inv n)]
                    (loop [i 0]
                      (when (< i n)
                        (aset arr i (rem (* (aget arr i) n-inv) mod))
                        (recur (inc i)))))))]
        (loop [i 0]
          (when (<= i deg-a)
            (aset a i (Long/parseLong (nth toks (+ 2 i))))
            (recur (inc i))))
        (loop [i 0]
          (when (<= i deg-b)
            (aset b i (Long/parseLong (nth toks (+ 3 deg-a i))))
            (recur (inc i))))
        (ntt a false)
        (ntt b false)
        (loop [i 0]
          (when (< i n)
            (aset a i (rem (* (aget a i) (aget b i)) mod))
            (recur (inc i))))
        (ntt a true)
        (let [final-deg (+ deg-a deg-b)
              sb (StringBuilder.)]
          (println final-deg)
          (loop [i 0]
            (when (<= i final-deg)
              (when (> i 0)
                (.append sb " "))
              (.append sb (aget a i))
              (recur (inc i))))
          (println (.toString sb)))))))
