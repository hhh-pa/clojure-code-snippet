(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n (Integer/parseInt (nth toks 0))
          q (Integer/parseInt (nth toks 1))
          ^longs a (long-array n)
          _ (loop [i 0] (when (< i n) (aset a i (Long/parseLong (nth toks (+ 2 i)))) (recur (inc i))))
          sz (* 4 (max 1 n))
          ^longs tree (long-array sz)
          ^longs lazy (long-array sz)]
      (letfn [(build [node l r]
                (if (= l r)
                  (aset tree node (aget a l))
                  (let [m (+ l (quot (- r l) 2))]
                    (build (* 2 node) l m)
                    (build (inc (* 2 node)) (inc m) r)
                    (aset tree node (+ (aget tree (* 2 node)) (aget tree (inc (* 2 node))))))))
              (push-down [node l r]
                (let [val (aget lazy node)]
                  (when (not (zero? val))
                    (let [m (+ l (quot (- r l) 2))
                          left (* 2 node)
                          right (inc left)]
                      (aset tree left (+ (aget tree left) (* val (long (inc (- m l))))))
                      (aset lazy left (+ (aget lazy left) val))
                      (aset tree right (+ (aget tree right) (* val (long (- r m)))))
                      (aset lazy right (+ (aget lazy right) val))
                      (aset lazy node 0)))))
              (update-range [node l r ql qr val]
                (if (and (<= ql l) (<= r qr))
                  (do
                    (aset tree node (+ (aget tree node) (* val (long (inc (- r l))))))
                    (aset lazy node (+ (aget lazy node) val)))
                  (do
                    (push-down node l r)
                    (let [m (+ l (quot (- r l) 2))]
                      (when (<= ql m)
                        (update-range (* 2 node) l m ql qr val))
                      (when (> qr m)
                        (update-range (inc (* 2 node)) (inc m) r ql qr val))
                      (aset tree node (+ (aget tree (* 2 node)) (aget tree (inc (* 2 node)))))))))
              (query-range [node l r ql qr]
                (if (and (<= ql l) (<= r qr))
                  (aget tree node)
                  (do
                    (push-down node l r)
                    (let [m (+ l (quot (- r l) 2))
                          s1 (if (<= ql m) (query-range (* 2 node) l m ql qr) 0)
                          s2 (if (> qr m) (query-range (inc (* 2 node)) (inc m) r ql qr) 0)]
                      (+ s1 s2)))))]
        (when (> n 0) (build 1 0 (dec n)))
        (let [sb (StringBuilder.)]
          (loop [i 0, ti (+ 2 n)]
            (when (and (< i q) (< ti nt))
              (let [op (nth toks ti)]
                (cond
                  (= op "UPDATE")
                  (let [l (Integer/parseInt (nth toks (+ ti 1)))
                        r (Integer/parseInt (nth toks (+ ti 2)))
                        val (Long/parseLong (nth toks (+ ti 3)))]
                    (update-range 1 0 (dec n) (dec l) (dec r) val)
                    (recur (inc i) (+ ti 4)))
                  (= op "QUERY")
                  (let [l (Integer/parseInt (nth toks (+ ti 1)))
                        r (Integer/parseInt (nth toks (+ ti 2)))]
                    (.append sb (str (query-range 1 0 (dec n) (dec l) (dec r)) "\n"))
                    (recur (inc i) (+ ti 3)))
                  :else
                  (recur (inc i) (inc ti))))))
          (print (.toString sb)))))))
