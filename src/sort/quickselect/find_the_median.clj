(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          a (int-array n)]
      (dotimes [i n]
        (aset-int a i (Integer/parseInt (aget ^objects toks (unchecked-inc-int i)))))
      (let [k (quot n 2)]
        (loop [l (int 0) r (int (dec n))]
          (if (>= l r)
            (println (aget a k))
            (let [pivot-idx (+ l (rand-int (- (inc r) l)))
                  pivot (aget a pivot-idx)]
              (aset-int a pivot-idx (aget a r))
              (aset-int a r pivot)
              (let [mid (loop [i l idx l]
                          (if (< i r)
                            (if (< (aget a i) pivot)
                              (do (let [tmp (aget a i)]
                                    (aset-int a i (aget a idx))
                                    (aset-int a idx tmp))
                                  (recur (inc i) (inc idx)))
                              (recur (inc i) idx))
                            idx))]
                (aset-int a r (aget a mid))
                (aset-int a mid pivot)
                (cond
                  (= mid k) (println pivot)
                  (< mid k) (recur (inc mid) r)
                  :else (recur l (dec mid)))))))))))
