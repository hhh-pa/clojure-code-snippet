(letfn [(height [node] (if node (int (:ht node)) -1))
        (update-ht [node]
          (assoc node :ht (int (inc (max (height (:left node)) (height (:right node)))))))
        (bf [node] (if node (int (- (height (:left node)) (height (:right node)))) 0))
        (rotate-right [y]
          (let [x (:left y)
                t2 (:right x)
                ny (update-ht (assoc y :left t2))
                nx (update-ht (assoc x :right ny))]
            nx))
        (rotate-left [x]
          (let [y (:right x)
                t2 (:left y)
                nx (update-ht (assoc x :right t2))
                ny (update-ht (assoc y :left nx))]
            ny))
        (insert [node val]
          (if (nil? node)
            {:val val :left nil :right nil :ht 0}
            (let [node (if (< val (int (:val node)))
                         (assoc node :left (insert (:left node) val))
                         (assoc node :right (insert (:right node) val)))
                  node (update-ht node)
                  b (bf node)]
              (cond
                (> b 1)
                (let [node (if (> val (int (:val (:left node))))
                             (assoc node :left (rotate-left (:left node)))
                             node)]
                  (rotate-right node))
                (< b -1)
                (let [node (if (< val (int (:val (:right node))))
                             (assoc node :right (rotate-right (:right node)))
                             node)]
                  (rotate-left node))
                :else node))))
        (inorder [node]
          (when node
            (str (inorder (:left node))
                 (:val node) "(BF=" (bf node) ") "
                 (inorder (:right node)))))
        (preorder [node]
          (when node
            (str (:val node) "(BF=" (bf node) ") "
                 (preorder (:left node))
                 (preorder (:right node)))))]
  (let [s (slurp *in*)]
    (when-not (clojure.string/blank? s)
      (let [toks (.split (.trim ^String s) "\\s+")
            n (Integer/parseInt (aget ^objects toks 0))
            root (loop [i (int 0) cur nil]
                   (if (< i n)
                     (recur (inc i) (insert cur (Integer/parseInt (aget ^objects toks (inc i)))))
                     cur))
            new-val (Integer/parseInt (aget ^objects toks (inc n)))
            root (insert root new-val)]
        (println (inorder root))
        (println (preorder root))))))
