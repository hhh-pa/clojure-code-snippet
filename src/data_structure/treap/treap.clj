(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          q (Integer/parseInt (nth toks 0))
          cap (max 20 (+ q 5))
          ^ints key-arr (int-array cap)
          ^ints prio-arr (int-array cap)
          ^ints sz-arr (int-array cap)
          ^ints left-arr (int-array cap -1)
          ^ints right-arr (int-array cap -1)
          root-ref (int-array [-1])
          node-cnt (int-array [0])
          rng (java.util.Random. 1337)
          sb (StringBuilder.)]

      (letfn [(get-sz [t]
                (if (== t -1) 0 (aget sz-arr t)))

              (update-sz [t]
                (when (not= t -1)
                  (aset sz-arr t (inc (+ (get-sz (aget left-arr t))
                                         (get-sz (aget right-arr t)))))))

              (split [t key]
                (if (== t -1)
                  [-1 -1]
                  (if (< key (aget key-arr t))
                    (let [[ll lr] (split (aget left-arr t) key)]
                      (aset left-arr t lr)
                      (update-sz t)
                      [ll t])
                    (let [[rl rr] (split (aget right-arr t) key)]
                      (aset right-arr t rl)
                      (update-sz t)
                      [t rr]))))

              (merge-nodes [l r]
                (cond
                  (== l -1) r
                  (== r -1) l
                  (> (aget prio-arr l) (aget prio-arr r))
                  (do
                    (aset right-arr l (merge-nodes (aget right-arr l) r))
                    (update-sz l)
                    l)
                  :else
                  (do
                    (aset left-arr r (merge-nodes l (aget left-arr r)))
                    (update-sz r)
                    r)))

              (insert [k]
                (let [[l r] (split (aget root-ref 0) k)
                      exists? (loop [cur l]
                                (cond
                                  (== cur -1) false
                                  (== (aget right-arr cur) -1) (== (aget key-arr cur) k)
                                  :else (recur (aget right-arr cur))))]
                  (if exists?
                    (aset root-ref 0 (merge-nodes l r))
                    (let [idx (aget node-cnt 0)]
                      (aset node-cnt 0 (inc idx))
                      (aset key-arr idx k)
                      (aset prio-arr idx (bit-and (.nextInt rng) 0x7FFFFFFF))
                      (aset sz-arr idx 1)
                      (aset left-arr idx -1)
                      (aset right-arr idx -1)
                      (let [new-l (merge-nodes l idx)]
                        (aset root-ref 0 (merge-nodes new-l r)))))))

              (find-key [k]
                (loop [cur (aget root-ref 0)]
                  (cond
                    (== cur -1) false
                    (== (aget key-arr cur) k) true
                    (< k (aget key-arr cur)) (recur (aget left-arr cur))
                    :else (recur (aget right-arr cur)))))

              (erase-key [k]
                (let [[l r] (split (aget root-ref 0) (dec k))
                      [_ rr] (split r k)]
                  (aset root-ref 0 (merge-nodes l rr))))

              (inorder [t first-box]
                (when (not= t -1)
                  (inorder (aget left-arr t) first-box)
                  (if (aget ^booleans first-box 0)
                    (do
                      (.append sb (str (aget key-arr t)))
                      (aset ^booleans first-box 0 false))
                    (.append sb (str " " (aget key-arr t))))
                  (inorder (aget right-arr t) first-box)))]

        (loop [i 0, ti 1]
          (when (and (< i q) (< ti nt))
            (let [op (nth toks ti)]
              (cond
                (= op "INSERT")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (insert val)
                  (recur (inc i) (+ ti 2)))

                (= op "FIND")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (.append sb (if (find-key val) "FOUND\n" "NOT_FOUND\n"))
                  (recur (inc i) (+ ti 2)))

                (= op "ERASE")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (erase-key val)
                  (recur (inc i) (+ ti 2)))

                (= op "PRINT")
                (let [first-box (boolean-array [true])]
                  (inorder (aget root-ref 0) first-box)
                  (.append sb "\n")
                  (recur (inc i) (inc ti)))

                :else
                (recur (inc i) (inc ti))))))
        (print (.toString sb))))))
