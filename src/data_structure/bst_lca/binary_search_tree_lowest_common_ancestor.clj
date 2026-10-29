(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          v1 (Integer/parseInt (aget ^objects toks (inc n)))
          v2 (Integer/parseInt (aget ^objects toks (+ 2 n)))
          lo (min v1 v2)
          hi (max v1 v2)
          insert (fn insert [tree v]
                   (if (nil? tree)
                     {:val v :left nil :right nil}
                     (if (< v (:val tree))
                       (assoc tree :left (insert (:left tree) v))
                       (assoc tree :right (insert (:right tree) v)))))
          root (loop [i (int 0) t nil]
                 (if (< i n)
                   (recur (inc i) (insert t (Integer/parseInt (aget ^objects toks (inc i)))))
                   t))]
      (loop [curr root]
        (cond
          (nil? curr) nil
          (< hi (:val curr)) (recur (:left curr))
          (> lo (:val curr)) (recur (:right curr))
          :else (println (:val curr)))))))
