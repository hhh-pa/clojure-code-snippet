(defn find-root [^ints parent x]
  (let [r (loop [r x]
            (let [p (aget parent r)]
              (if (= p r) r (recur p))))]
    (loop [x x]
      (let [p (aget parent x)]
        (when (not (= p r))
          (aset parent x (int r))
          (recur p))))
    r))

(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n (Integer/parseInt (nth toks 0))
          ^ints parent (int-array n)
          _ (loop [i 0] (when (< i n) (aset parent i i) (recur (inc i))))
          comps (atom n)
          sb (StringBuilder.)]
      (loop [i 1]
        (when (< (inc i) nt)
          (let [u (dec (Integer/parseInt (nth toks i)))
                v (dec (Integer/parseInt (nth toks (inc i))))
                ru (find-root parent u)
                rv (find-root parent v)]
            (if (= ru rv)
              (do (.append sb (str @comps " CYCLE FORMED!\n")))
              (do (aset parent rv (int ru))
                  (swap! comps dec)
                  (.append sb (str @comps "\n"))))
            (recur (+ i 2)))))
      (print (.toString sb)))))
