;; Sudoku solver: Knuth's Algorithm X over dancing links (exact cover).
;; 324 columns (81 cell + 81 row-digit + 81 col-digit + 81 box-digit) and
;; 729 rows, one per (cell, digit) quadruple, as toroidal doubly-linked
;; lists in int arrays with cover/uncover. Column choice is min (size,
;; index) over uncovered CELL columns and rows are tried digit-ascending,
;; which is the MRV backtracker's expansion order, so this finds the same
;; first solution on every input (verified by byte-compare) while visiting
;; no more search nodes. The static matrix is built once per process; each
;; puzzle gets fresh mutable arrays via System/arraycopy (C/RC/RD are
;; read-only, shared). Uncovered cell columns live in a packed list for a
;; short column-choice scan. Each exact-cover row owns 4 consecutive node
;; ids [cell, row-digit, col-digit, box-digit], so cover/uncover visit a
;; row's other nodes by direct unrolled index (RB maps node to row base)
;; instead of L/R traversal; offset 0 is always the cell node, hence the
;; only S update (S is only read for cell columns).
(require '[clojure.string :as str])
;; All arithmetic below is small (indices < 3241, sizes < 10): overflow
;; checks are dead weight, so compile the rest of the file unchecked.
(set! *unchecked-math* true)
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
          sb (StringBuilder.)
          ;; Static exact-cover template (built once, see below).
          ^ints L0 (int-array 3241)
          ^ints R0 (int-array 3241)
          ^ints U0 (int-array 3241)
          ^ints D0 (int-array 3241)
          ^ints C (int-array 3241)
          ^ints S0 (int-array 325)
          ^ints PL0 (int-array 81)
          ^ints PP0 (int-array 82)
          ^ints RC (int-array 3241)
          ^ints RD (int-array 3241)
          ^ints RB (int-array 3241)]
      ;; Header row: node 0 is the root, 1..324 the column headers.
      (aset L0 0 324)
      (aset R0 0 1)
      (loop [i 1]
        (when (<= i 324)
          (aset L0 i (dec i))
          (aset R0 i (if (= i 324) 0 (inc i)))
          (aset U0 i i)
          (aset D0 i i)
          (aset C i i)
          (when (<= i 81)
            (aset PL0 (dec i) i)
            (aset PP0 i (dec i)))
          (recur (inc i))))
      ;; Data rows: cells row-major, digits ascending, so each cell
      ;; column's D-chain tries digits 1..9 in order.
      (loop [k 0]
        (when (< k 729)
          (let [idx (quot k 9)
                d (inc (rem k 9))
                m (+ 325 (* k 4))
                r (quot idx 9)
                c (rem idx 9)
                x (+ (* (quot r 3) 3) (quot c 3))
                c0 (inc idx)
                c1 (+ 82 (* r 9) (dec d))
                c2 (+ 163 (* c 9) (dec d))
                c3 (+ 244 (* x 9) (dec d))]
            ;; Link the 4 nodes of this row left/right.
            (aset L0 m (+ m 3)) (aset R0 m (+ m 1))
            (aset L0 (+ m 1) m) (aset R0 (+ m 1) (+ m 2))
            (aset L0 (+ m 2) (+ m 1)) (aset R0 (+ m 2) (+ m 3))
            (aset L0 (+ m 3) (+ m 2)) (aset R0 (+ m 3) m)
            (aset RB m m)
            (aset RB (+ m 1) m)
            (aset RB (+ m 2) m)
            (aset RB (+ m 3) m)
            ;; Append each node to the bottom of its column.
            (let [m1 (inc m)
                  m2 (+ m 2)
                  m3 (+ m 3)
                  u0 (aget U0 c0)
                  u1 (aget U0 c1)
                  u2 (aget U0 c2)
                  u3 (aget U0 c3)]
              (aset U0 m u0)
              (aset D0 m c0)
              (aset D0 u0 m)
              (aset U0 c0 m)
              (aset C m c0)
              (aset S0 c0 (inc (aget S0 c0)))
              (aset RC m idx)
              (aset RD m d)
              (aset U0 m1 u1)
              (aset D0 m1 c1)
              (aset D0 u1 m1)
              (aset U0 c1 m1)
              (aset C m1 c1)
              (aset S0 c1 (inc (aget S0 c1)))
              (aset RC m1 idx)
              (aset RD m1 d)
              (aset U0 m2 u2)
              (aset D0 m2 c2)
              (aset D0 u2 m2)
              (aset U0 c2 m2)
              (aset C m2 c2)
              (aset S0 c2 (inc (aget S0 c2)))
              (aset RC m2 idx)
              (aset RD m2 d)
              (aset U0 m3 u3)
              (aset D0 m3 c3)
              (aset D0 u3 m3)
              (aset U0 c3 m3)
              (aset C m3 c3)
              (aset S0 c3 (inc (aget S0 c3)))
              (aset RC m3 idx)
              (aset RD m3 d)))
          (recur (inc k))))
      (loop [p 0]
        (when (< p t)
          (let [base (+ 1 (* p 81))
                ^ints L (int-array 3241)
                ^ints R (int-array 3241)
                ^ints U (int-array 3241)
                ^ints D (int-array 3241)
                ^ints S (int-array 325)
                ^ints PL (int-array 81)
                ^ints PP (int-array 82)
                ^ints PN (int-array 1)
                ^ints O (int-array 81)
                ^ints b (int-array 81)]
            ;; Fresh mutable links per puzzle; C/RC/RD shared read-only.
            (System/arraycopy L0 0 L 0 3241)
            (System/arraycopy R0 0 R 0 3241)
            (System/arraycopy U0 0 U 0 3241)
            (System/arraycopy D0 0 D 0 3241)
            (System/arraycopy S0 0 S 0 325)
            (System/arraycopy PL0 0 PL 0 81)
            (System/arraycopy PP0 0 PP 0 82)
            (aset PN 0 81)
            ;; Cell/column variants are split (every call site knows the
            ;; column kind statically) to skip the dispatch branch.
            (letfn [(cover-cell! [c]
                      (aset R (aget L c) (aget R c))
                      (aset L (aget R c) (aget L c))
                      (let [pn (dec (aget PN 0))]
                        (aset PN 0 pn)
                        (let [pos (aget PP c)
                              last (aget PL pn)]
                          (aset PL pos last)
                          (aset PP last pos)))
                      (loop [i (aget D c)]
                        (when (not (= i c))
                          (let [b (aget RB i)
                                j1 (inc b)
                                j2 (+ b 2)
                                j3 (+ b 3)
                                u1 (aget U j1) d1 (aget D j1)
                                u2 (aget U j2) d2 (aget D j2)
                                u3 (aget U j3) d3 (aget D j3)]
                            (aset D u1 d1)
                            (aset U d1 u1)
                            (aset D u2 d2)
                            (aset U d2 u2)
                            (aset D u3 d3)
                            (aset U d3 u3))
                          (recur (aget D i)))))
                    (cover-con! [c]
                      (aset R (aget L c) (aget R c))
                      (aset L (aget R c) (aget L c))
                      (loop [i (aget D c)]
                        (when (not (= i c))
                          (let [b (aget RB i)
                                cc0 (aget C b)
                                u0 (aget U b) d0 (aget D b)]
                            (aset D u0 d0)
                            (aset U d0 u0)
                            (aset S cc0 (dec (aget S cc0)))
                            (let [j1 (inc b)
                                  j2 (+ b 2)
                                  j3 (+ b 3)]
                              (cond
                                (= i j1)
                                (let [u2 (aget U j2) d2 (aget D j2)
                                      u3 (aget U j3) d3 (aget D j3)]
                                  (aset D u2 d2)
                                  (aset U d2 u2)
                                  (aset D u3 d3)
                                  (aset U d3 u3))
                                (= i j2)
                                (let [u1 (aget U j1) d1 (aget D j1)
                                      u3 (aget U j3) d3 (aget D j3)]
                                  (aset D u1 d1)
                                  (aset U d1 u1)
                                  (aset D u3 d3)
                                  (aset U d3 u3))
                                :else
                                (let [u1 (aget U j1) d1 (aget D j1)
                                      u2 (aget U j2) d2 (aget D j2)]
                                  (aset D u1 d1)
                                  (aset U d1 u1)
                                  (aset D u2 d2)
                                  (aset U d2 u2)))))
                          (recur (aget D i)))))
                    (uncover-cell! [c]
                      (loop [i (aget U c)]
                        (when (not (= i c))
                          (let [b (aget RB i)
                                j1 (inc b)
                                j2 (+ b 2)
                                j3 (+ b 3)
                                u1 (aget U j1) d1 (aget D j1)
                                u2 (aget U j2) d2 (aget D j2)
                                u3 (aget U j3) d3 (aget D j3)]
                            (aset D u1 j1)
                            (aset U d1 j1)
                            (aset D u2 j2)
                            (aset U d2 j2)
                            (aset D u3 j3)
                            (aset U d3 j3))
                          (recur (aget U i))))
                      (aset R (aget L c) (int c))
                      (aset L (aget R c) (int c))
                      (let [pn (aget PN 0)]
                        (aset PL pn (int c))
                        (aset PP c pn)
                        (aset PN 0 (inc pn))))
                    (uncover-con! [c]
                      (loop [i (aget U c)]
                        (when (not (= i c))
                          (let [b (aget RB i)
                                cc0 (aget C b)
                                u0 (aget U b) d0 (aget D b)]
                            (aset D u0 b)
                            (aset U d0 b)
                            (aset S cc0 (inc (aget S cc0)))
                            (let [j1 (inc b)
                                  j2 (+ b 2)
                                  j3 (+ b 3)]
                              (cond
                                (= i j1)
                                (let [u2 (aget U j2) d2 (aget D j2)
                                      u3 (aget U j3) d3 (aget D j3)]
                                  (aset D u2 j2)
                                  (aset U d2 j2)
                                  (aset D u3 j3)
                                  (aset U d3 j3))
                                (= i j2)
                                (let [u1 (aget U j1) d1 (aget D j1)
                                      u3 (aget U j3) d3 (aget D j3)]
                                  (aset D u1 j1)
                                  (aset U d1 j1)
                                  (aset D u3 j3)
                                  (aset U d3 j3))
                                :else
                                (let [u1 (aget U j1) d1 (aget D j1)
                                      u2 (aget U j2) d2 (aget D j2)]
                                  (aset D u1 j1)
                                  (aset U d1 j1)
                                  (aset D u2 j2)
                                  (aset U d2 j2)))))
                          (recur (aget U i))))
                      (aset R (aget L c) (int c))
                      (aset L (aget R c) (int c)))
                    ;; Exact min (size, index) over the packed uncovered
                    ;; cell columns: the MRV backtracker's best cell (any
                    ;; size-0 column means dead, so the first one returns).
                    (choose-cell []
                      (let [n (aget PN 0)]
                        (loop [i 0 best -1 bs 10]
                          (if (>= i n)
                            best
                            (let [cc (aget PL i)
                                  s (aget S cc)]
                              (cond
                                (zero? s) cc
                                (or (< s bs) (and (= s bs) (< cc best)))
                                (recur (inc i) cc s)
                                :else (recur (inc i) best bs)))))))
                    (search! [k]
                      (let [c (choose-cell)]
                        (cond
                          (= c -1) true
                          (zero? (aget S c)) false
                          :else
                          (do
                            (cover-cell! c)
                            ;; rr is in a cell column, hence row offset 0:
                            ;; its side columns sit at rr+1..rr+3.
                            (let [ok (loop [rr (aget D c)]
                                       (if (= rr c)
                                         false
                                         (let [nx (aget D rr)]
                                           (aset O k rr)
                                           (cover-con! (aget C (inc rr)))
                                           (cover-con! (aget C (+ rr 2)))
                                           (cover-con! (aget C (+ rr 3)))
                                           (if (search! (inc k))
                                             true
                                             (do
                                               (uncover-con! (aget C (+ rr 3)))
                                               (uncover-con! (aget C (+ rr 2)))
                                               (uncover-con! (aget C (inc rr)))
                                               (recur nx))))))]
                              (when-not ok
                                (uncover-cell! c))
                              ok)))))]
              ;; Cover the given clues and push them on the solution stack.
              (let [ng (loop [idx 0 g 0]
                         (if (>= idx 81)
                           g
                           (let [v (aget all (+ base idx))]
                             (aset b idx v)
                             (if (zero? v)
                               (recur (inc idx) g)
                               (do
                                 (let [n0 (+ 325 (* (+ (* idx 9) (dec v)) 4))]
                                   (aset O g n0)
                                   (cover-cell! (aget C n0))
                                   (cover-con! (aget C (inc n0)))
                                   (cover-con! (aget C (+ n0 2)))
                                   (cover-con! (aget C (+ n0 3))))
                                 (recur (inc idx) (inc g)))))))]
                (when (search! ng)
                  ;; Decode the non-given rows (O[0..ng) are the givens,
                  ;; already in the board) into the board.
                  (loop [s ng]
                    (when (< s 81)
                      (let [n (aget O s)]
                        (aset b (aget RC n) (aget RD n)))
                      (recur (inc s)))))))
            ;; On failure the board still holds givens+zeros, matching
            ;; the MRV solver's fully-backtracked board.
            (loop [r 0]
              (when (< r 9)
                (loop [c 0]
                  (when (< c 9)
                    (.append sb (str (aget b (+ (* r 9) c)) " "))
                    (recur (inc c))))
                (.append sb "\n")
                (recur (inc r)))))
          (recur (inc p))))
      (print (str sb)))))
