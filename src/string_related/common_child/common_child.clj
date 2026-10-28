(let [lines (line-seq (java.io.BufferedReader. *in*))]
  (when (seq lines)
    (let [s1 ^String (first lines)
          s2 ^String (second lines)
          n (int (count s1))
          m (int (count s2))
          prev (int-array (inc m))
          curr (int-array (inc m))]
      (dotimes [i n]
        (let [c1 (int (.charAt s1 i))]
          (dotimes [j m]
            (let [c2 (int (.charAt s2 j))]
              (if (= c1 c2)
                (aset-int curr (inc j) (inc (aget prev j)))
                (aset-int curr (inc j) (max (aget prev (inc j)) (aget curr j))))))
          (System/arraycopy curr 0 prev 0 (inc m))))
      (println (aget prev m)))))
