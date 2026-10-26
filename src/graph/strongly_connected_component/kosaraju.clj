(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [n (int (count s))
          data (loop [i (int 0) c (int 0) ^ints a (int-array 1024)]
                 (if (>= i n)
                   (java.util.Arrays/copyOf a c)
                   (let [ch (int (.charAt ^String s i))]
                     (if (and (>= ch 48) (<= ch 57))
                       (let [r (loop [j (int i) v (int 0)]
                                 (if (< j n)
                                   (let [d (int (.charAt ^String s j))]
                                     (if (and (>= d 48) (<= d 57))
                                       (recur (unchecked-inc-int j)
                                              (unchecked-add-int
                                               (unchecked-multiply-int v (int 10))
                                               (unchecked-subtract-int d (int 48))))
                                       [j v]))
                                   [j v]))
                             j (int (nth r 0)) v (int (nth r 1))
                             a (if (>= c (alength a))
                                 (java.util.Arrays/copyOf a (unchecked-multiply-int (alength a) (int 2)))
                                 a)]
                         (aset-int a c v)
                         (recur j (unchecked-inc-int c) a))
                       (recur (unchecked-inc-int i) c a)))))
          ^ints d data
          nd (alength d)]
      (when (>= nd 2)
        (let [nn (aget d 0)
              ;; like the reference, consume (a b) pairs until EOF (m ignored)
              npairs (quot (- nd 2) 2)
              g (make-array java.util.ArrayList nn)
              rg (make-array java.util.ArrayList nn)]
          (dotimes [i nn]
            (aset g i (java.util.ArrayList.))
            (aset rg i (java.util.ArrayList.)))
          (dotimes [k npairs]
            (let [a (unchecked-dec-int (aget d (+ 2 (* k 2))))
                  b (unchecked-dec-int (aget d (+ 3 (* k 2))))]
              (when (and (>= a 0) (< a nn) (>= b 0) (< b nn))
                (.add ^java.util.ArrayList (aget g a) (Integer. b))
                (.add ^java.util.ArrayList (aget rg b) (Integer. a)))))
          ;; pass 1: iterative DFS on reversed graph, post-order.
          ;; Faithful simulation of the recursive loop: each stack frame keeps
          ;; its next-child index, children are pushed one at a time. (Pushing
          ;; all children upfront with exit markers can mark a node before a
          ;; descendant edge reaches it, producing an invalid post-order.)
          (let [vis (boolean-array nn)
                order (java.util.ArrayList. ^int nn)
                nst (java.util.ArrayDeque.)
                ist (java.util.ArrayDeque.)]
            (dotimes [v nn]
              (when-not (aget ^booleans vis v)
                (aset-boolean vis v true)
                (.push ^java.util.ArrayDeque nst (Integer. v))
                (.push ^java.util.ArrayDeque ist (Integer. 0))
                (while (not (.isEmpty ^java.util.ArrayDeque nst))
                  (let [u (int (.peek ^java.util.ArrayDeque nst))
                        i (int (.peek ^java.util.ArrayDeque ist))
                        nbrs ^java.util.ArrayList (aget rg u)]
                    (if (< i (.size nbrs))
                      (let [w (int (.get nbrs i))]
                        (.pop ^java.util.ArrayDeque ist)
                        (.push ^java.util.ArrayDeque ist (Integer. (unchecked-inc-int i)))
                        (when-not (aget ^booleans vis w)
                          (aset-boolean vis w true)
                          (.push ^java.util.ArrayDeque nst (Integer. w))
                          (.push ^java.util.ArrayDeque ist (Integer. 0))))
                      (do
                        (.pop ^java.util.ArrayDeque nst)
                        (.pop ^java.util.ArrayDeque ist)
                        (.add ^java.util.ArrayList order (Integer. u))))))))
            ;; pass 2: pop post-order, DFS on g counting component sizes
            (java.util.Arrays/fill ^booleans vis false)
            (let [odd (atom 0) even (atom 0)
                  st2 (java.util.ArrayDeque.)]
              (loop [idx (unchecked-dec-int (.size ^java.util.ArrayList order))]
                (when (>= idx 0)
                  (let [v (int (.get ^java.util.ArrayList order idx))]
                    (when-not (aget ^booleans vis v)
                      (aset-boolean vis v true)
                      (.push ^java.util.ArrayDeque st2 (Integer. v))
                      (loop [cnt (int 0)]
                        (if (.isEmpty ^java.util.ArrayDeque st2)
                          (if (odd? cnt)
                            (swap! odd + cnt)
                            (swap! even + cnt))
                          (let [x (int (.pop ^java.util.ArrayDeque st2))
                                nbrs ^java.util.ArrayList (aget g x)
                                cnt (unchecked-inc-int cnt)]
                            (loop [j (int 0)]
                              (when (< j (.size nbrs))
                                (let [w (int (.get nbrs j))]
                                  (when-not (aget ^booleans vis w)
                                    (aset-boolean vis w true)
                                    (.push ^java.util.ArrayDeque st2 (Integer. w))))
                                (recur (unchecked-inc-int j))))
                            (recur cnt))))))
                  (recur (unchecked-dec-int idx))))
              (print (str (- @odd @even) "\n")))))))))
