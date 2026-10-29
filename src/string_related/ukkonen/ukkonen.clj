(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          text (.next toks)
          n (.length text)
          tree-children (java.util.ArrayList.)
          tree-suffix-link (java.util.ArrayList.)
          tree-start (java.util.ArrayList.)
          tree-end (java.util.ArrayList.)
          tree-is-leaf (java.util.ArrayList.)
          leaf-end (volatile! -1)
          root 0
          active-node (volatile! 0)
          active-edge (volatile! -1)
          active-length (volatile! 0)
          remaining-suffix-count (volatile! 0)
          new-node (fn [start end is-leaf]
                     (.add tree-children (java.util.HashMap.))
                     (.add tree-suffix-link root)
                     (.add tree-start (int start))
                     (.add tree-end (int end))
                     (.add tree-is-leaf (boolean is-leaf))
                     (dec (.size tree-children)))
          get-end (fn [n-idx]
                    (if (.get tree-is-leaf (int n-idx))
                      @leaf-end
                      (int (.get tree-end (int n-idx)))))
          edge-length (fn [n-idx]
                        (if (= (int n-idx) root)
                          0
                          (inc (- (get-end n-idx) (int (.get tree-start (int n-idx)))))))
          walk-down (fn [curr-node]
                      (let [elen (edge-length curr-node)]
                        (if (>= @active-length elen)
                          (do
                            (vswap! active-edge + elen)
                            (vswap! active-length - elen)
                            (vreset! active-node curr-node)
                            true)
                          false)))]
      (new-node 0 -1 false)
      (dotimes [pos n]
        (vreset! leaf-end pos)
        (vswap! remaining-suffix-count inc)
        (let [last-new-node (volatile! -1)]
          (loop []
            (when (pos? @remaining-suffix-count)
              (when (zero? @active-length)
                (vreset! active-edge pos))
              (let [cur-char (.charAt text @active-edge)
                    children ^java.util.HashMap (.get tree-children @active-node)
                    next-node (.get children cur-char)]
                (if (nil? next-node)
                  (do
                    (let [leaf (new-node pos -1 true)]
                      (.put children cur-char leaf)
                      (when (not= @last-new-node -1)
                        (.set tree-suffix-link @last-new-node @active-node)
                        (vreset! last-new-node -1)))
                    (vswap! remaining-suffix-count dec)
                    (if (and (= @active-node root) (pos? @active-length))
                      (do
                        (vswap! active-length dec)
                        (vreset! active-edge (+ (- pos @remaining-suffix-count) 1)))
                      (when (not= @active-node root)
                        (vreset! active-node (.get tree-suffix-link @active-node))))
                    (recur))
                  (if (walk-down next-node)
                    (recur)
                    (let [edge-char-pos (+ (int (.get tree-start (int next-node))) @active-length)]
                      (if (= (.charAt text edge-char-pos) (.charAt text pos))
                        (do
                          (when (and (not= @last-new-node -1) (not= @active-node root))
                            (.set tree-suffix-link @last-new-node @active-node)
                            (vreset! last-new-node -1))
                          (vswap! active-length inc))
                        (do
                          (let [split-end (dec (+ (int (.get tree-start (int next-node))) @active-length))
                                split (new-node (.get tree-start (int next-node)) split-end false)
                                split-children ^java.util.HashMap (.get tree-children split)
                                leaf (new-node pos -1 true)]
                            (.put children cur-char split)
                            (.put split-children (.charAt text pos) leaf)
                            (.set tree-start (int next-node) (+ (int (.get tree-start (int next-node))) @active-length))
                            (.put split-children (.charAt text edge-char-pos) next-node)
                            (when (not= @last-new-node -1)
                              (.set tree-suffix-link @last-new-node split))
                            (vreset! last-new-node split))
                          (vswap! remaining-suffix-count dec)
                          (if (and (= @active-node root) (pos? @active-length))
                            (do
                              (vswap! active-length dec)
                              (vreset! active-edge (+ (- pos @remaining-suffix-count) 1)))
                            (when (not= @active-node root)
                              (vreset! active-node (.get tree-suffix-link @active-node))))
                          (recur)))))))))))
      (let [count-distinct (fn count-fn [u]
                             (let [children ^java.util.HashMap (.get tree-children (int u))]
                               (reduce (fn [acc v]
                                         (let [len (inc (- (get-end v) (int (.get tree-start (int v)))))]
                                           (+ acc len (count-fn v))))
                                       0
                                       (.values children))))
            find-pattern (fn [^String pat]
                           (let [m (.length pat)]
                             (loop [cur root idx 0]
                               (if (< idx m)
                                 (let [c (.charAt pat idx)
                                       children ^java.util.HashMap (.get tree-children (int cur))
                                       nxt (.get children c)]
                                   (if (nil? nxt)
                                     false
                                     (let [start (int (.get tree-start (int nxt)))
                                           end (get-end nxt)
                                           len (inc (- end start))
                                           match-len (loop [k 0 cur-idx idx]
                                                       (if (and (< k len) (< cur-idx m))
                                                         (if (= (.charAt text (+ start k)) (.charAt pat cur-idx))
                                                           (recur (inc k) (inc cur-idx))
                                                           -1)
                                                         (- cur-idx idx)))]
                                       (if (= match-len -1)
                                         false
                                         (recur (int nxt) (+ idx match-len))))))
                                 true))))]
        (println (str "Nodes: " (.size tree-children)))
        (println (str "Distinct substrings: " (count-distinct root)))
        (when (.hasNextInt toks)
          (let [q (.nextInt toks)]
            (dotimes [_ q]
              (let [pat (.next toks)]
                (if (find-pattern pat)
                  (println "YES")
                  (println "NO"))))))))))
