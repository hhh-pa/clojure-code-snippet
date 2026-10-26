(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          data (java.util.ArrayList.)
          sb (StringBuilder.)]
      (loop [i 1]
        (when (< i (count toks))
          (let [op (nth toks i)]
            (cond
              (= op "i")
              (do
                (when (< (inc i) (count toks))
                  (let [v (Integer/parseInt (nth toks (inc i)))
                        pos (java.util.Collections/binarySearch data v)]
                    (.add data (if (neg? pos) (- (inc pos)) pos) v)))
                (recur (+ i 2)))
              (= op "r")
              (do
                (when (< (inc i) (count toks))
                  (let [v (Integer/parseInt (nth toks (inc i)))]
                    (if (.isEmpty data)
                      (.append sb "The tree is empty\n")
                      (let [pos (java.util.Collections/binarySearch data v)]
                        (if (neg? pos)
                          (do (.append sb (str "The key " v " is does not exist in the tree\n")))
                          (.remove data (int pos)))))))
                (recur (+ i 2)))
              (= op "t")
              (do
                (let [n (.size data)]
                  (loop [j 0]
                    (when (< j n)
                      (when (> j 0) (.append sb " "))
                      (.append sb (str (.get data j)))
                      (recur (inc j)))))
                (.append sb "\n")
                (recur (inc i)))
              :else (recur (inc i))))))
      (print (.toString sb)))))
