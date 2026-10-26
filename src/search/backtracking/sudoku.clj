;; Sudoku solver ported from the Go mirror (go/src/search/backtracking/sudoku.go):
;; MRV (minimum-remaining-values) backtracking, digits tried 1-9 in order.
;; NOTE: the shared expected outputs match this traversal exactly on all cases
;; (verified via a C++ re-implementation of the Go solver, 9/9 MATCH), while the
;; .cc row-major solver finds different valid solutions on the sparse puzzles.
(require '[clojure.string :as str])
(let [input (slurp *in*)]
  (when-not (str/blank? input)
    (let [tok (java.util.StringTokenizer. input)
          ^ints all (let [a (int-array (.countTokens tok))]
                      (loop [i 0]
                        (if (.hasMoreTokens tok)
                          (do (aset a i (Integer/parseInt (.nextToken tok)))
                              (recur (inc i)))
                          a)))
          len (alength all)
          t (if (pos? len) (min (aget all 0) (quot (dec len) 81)) 0)
          sb (StringBuilder.)]
      (letfn [(solve! [^ints b ^ints rs ^ints cs ^ints xs]
                (let [^ints bst (int-array [-1 -1 10 0])
                      dead (loop [r 0]
                             (cond
                               (or (>= r 9) (= (aget bst 2) 1)) false
                               :else
                               (let [d (loop [c 0]
                                         (cond
                                           (or (>= c 9) (= (aget bst 2) 1)) false
                                           :else
                                           (let [i (+ (* r 9) c)]
                                             (if (not (zero? (aget b i)))
                                               (recur (inc c))
                                               (let [box (+ (* (quot r 3) 3) (quot c 3))
                                                     used (bit-or (aget rs r) (aget cs c)
                                                                  (aget xs box))
                                                     p (loop [d 1 mask 0 cnt 0]
                                                         (if (> d 9)
                                                           (bit-or (bit-shift-left mask 4) cnt)
                                                           (if (zero? (bit-and used
                                                                               (bit-shift-left 1 d)))
                                                             (recur (inc d)
                                                                    (bit-or mask (bit-shift-left 1 d))
                                                                    (inc cnt))
                                                             (recur (inc d) mask cnt))))
                                                     cnt (bit-and p 15)]
                                                 (cond
                                                   (zero? cnt) true
                                                   (< cnt (aget bst 2))
                                                   (do (aset bst 0 r)
                                                       (aset bst 1 c)
                                                       (aset bst 2 cnt)
                                                       (aset bst 3 (bit-shift-right p 4))
                                                       (recur (inc c)))
                                                   :else (recur (inc c))))))))]
                                 (if d true (recur (inc r))))))]
                  (if dead
                    false
                    (let [br (aget bst 0)]
                      (if (= br -1)
                        true
                        (let [bc (aget bst 1)
                              bm (aget bst 3)
                              bi (+ (* br 9) bc)
                              box (+ (* (quot br 3) 3) (quot bc 3))]
                          (loop [d 1]
                            (if (> d 9)
                              false
                              (let [bit (bit-shift-left 1 d)]
                                (if (zero? (bit-and bm bit))
                                  (recur (inc d))
                                  (do
                                    (aset b bi d)
                                    (aset rs br (bit-or (aget rs br) bit))
                                    (aset cs bc (bit-or (aget cs bc) bit))
                                    (aset xs box (bit-or (aget xs box) bit))
                                    (if (solve! b rs cs xs)
                                      true
                                      (do
                                        (aset rs br (bit-and (aget rs br) (bit-not bit)))
                                        (aset cs bc (bit-and (aget cs bc) (bit-not bit)))
                                        (aset xs box (bit-and (aget xs box) (bit-not bit)))
                                        (aset b bi 0)
                                        (recur (inc d)))))))))))))))]
        (loop [p 0]
          (when (< p t)
            (let [base (+ 1 (* p 81))
                  ^ints b (int-array 81)
                  ^ints rs (int-array 9)
                  ^ints cs (int-array 9)
                  ^ints xs (int-array 9)]
              (loop [k 0]
                (when (< k 81)
                  (let [v (aget all (+ base k))
                        r (quot k 9)
                        c (rem k 9)]
                    (aset b k v)
                    (when (not (zero? v))
                      (let [bit (bit-shift-left 1 v)
                            bx (+ (* (quot r 3) 3) (quot c 3))]
                        (aset rs r (bit-or (aget rs r) bit))
                        (aset cs c (bit-or (aget cs c) bit))
                        (aset xs bx (bit-or (aget xs bx) bit)))))
                  (recur (inc k))))
              (solve! b rs cs xs)
              (loop [r 0]
                (when (< r 9)
                  (loop [c 0]
                    (when (< c 9)
                      (.append sb (str (aget b (+ (* r 9) c)) " "))
                      (recur (inc c))))
                  (.append sb "\n")
                  (recur (inc r)))))
            (recur (inc p))))
        (print (str sb))))))
