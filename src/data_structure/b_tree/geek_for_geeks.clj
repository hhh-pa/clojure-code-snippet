(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (alength toks)
          sb (StringBuilder.)]
      ;; Sorted multiset in a growable primitive int buffer instead of a
      ;; boxed ArrayList<Integer>; shifts use System/arraycopy and the
      ;; traversal appends ints directly (no per-element String).
      (loop [i 1, ^ints buf (int-array 16), sz 0]
        (if (>= i nt)
          (print (.toString sb))
          (let [op (aget toks i)]
            (cond
              (= op "i")
              (let [has-val (< (inc i) nt)
                    v (int (if has-val (Integer/parseInt (aget toks (inc i))) 0))
                    pos (if has-val
                          (java.util.Arrays/binarySearch buf (int 0) (int sz) v)
                          -1)
                    ins (if (neg? pos) (- (inc pos)) pos)
                    ^ints b2 (if (and has-val (== sz (alength buf)))
                               (java.util.Arrays/copyOf buf (* 2 (alength buf)))
                               buf)]
                (when has-val
                  (System/arraycopy b2 ins b2 (inc ins) (- sz ins))
                  (aset b2 ins v))
                (recur (+ i 2) b2 (if has-val (inc sz) sz)))
              (= op "r")
              (let [nsz (if (< (inc i) nt)
                          (let [v (Integer/parseInt (aget toks (inc i)))]
                            (if (zero? sz)
                              (do (.append sb "The tree is empty\n") sz)
                              (let [pos (java.util.Arrays/binarySearch buf (int 0) (int sz) v)]
                                (if (neg? pos)
                                  (do (.append sb (str "The key " v " is does not exist in the tree\n")) sz)
                                  (do (System/arraycopy buf (inc pos) buf pos (- (int sz) pos 1))
                                      (dec sz))))))
                          sz)]
                (recur (+ i 2) buf nsz))
              (= op "t")
              (do
                (loop [j 0]
                  (when (< j sz)
                    (when (pos? j) (.append sb " "))
                    (.append sb (aget buf j))
                    (recur (inc j))))
                (.append sb "\n")
                (recur (inc i) buf sz))
              :else (recur (inc i) buf sz))))))))
