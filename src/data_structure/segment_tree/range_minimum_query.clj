(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n (Integer/parseInt (nth toks 0))
          q (Integer/parseInt (nth toks 1))
          a (int-array n)
          _ (loop [i 0] (when (< i n) (aset a i (Integer/parseInt (nth toks (+ 2 i)))) (recur (inc i))))
          t (int-array (* 4 (max 1 n)))]
      (letfn [(build [node l r]
                (if (= l r)
                  (aset t node (aget a l))
                  (let [m (quot (+ l r) 2)]
                    (build (* 2 node) l m)
                    (build (inc (* 2 node)) (inc m) r)
                    (aset t node (min (aget t (* 2 node)) (aget t (inc (* 2 node))))))))
              (query [node l r ql qr]
                (cond
                  (or (< r ql) (< qr l)) Integer/MAX_VALUE
                  (and (<= ql l) (<= r qr)) (aget t node)
                  :else (let [m (quot (+ l r) 2)]
                          (min (query (* 2 node) l m ql qr)
                               (query (inc (* 2 node)) (inc m) r ql qr)))))
              (update [node l r idx val]
                (if (= l r)
                  (aset t node val)
                  (let [m (quot (+ l r) 2)]
                    (if (<= idx m)
                      (update (* 2 node) l m idx val)
                      (update (inc (* 2 node)) (inc m) r idx val))
                    (aset t node (min (aget t (* 2 node)) (aget t (inc (* 2 node))))))))]
        (when (> n 0) (build 1 0 (dec n)))
        (let [sb (StringBuilder.)]
          (loop [i 0, ti (+ 2 n)]
            (when (< i q)
              (if (>= (+ ti 2) nt)
                nil
                (let [typ (nth toks ti)
                      x (Integer/parseInt (nth toks (+ ti 1)))
                      y (Integer/parseInt (nth toks (+ ti 2)))]
                  (if (= typ "q")
                    (.append sb (str (query 1 0 (dec n) (dec x) (dec y)) "\n"))
                    (update 1 0 (dec n) (dec x) y))
                  (recur (inc i) (+ ti 3))))))
          (print (.toString sb)))))))
