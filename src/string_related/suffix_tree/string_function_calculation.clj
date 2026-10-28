;; String function calculation: max over all distinct substrings of
;; length * occurrences. Ukkonen's online suffix-tree construction for the
;; single lowercase input string plus a terminal sentinel; the answer is the
;; max over tree nodes of string-depth x subtree leaf count. Same
;; stdin/stdout contract as the suffix-automaton version in
;; src/string_related/suffix_array/string_function_calculation.clj.
;; Input: single string token. Output: the maximum (64-bit).
(require '[clojure.string :as str])
(set! *unchecked-math* true)

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          ^String s (first toks)
          n (int (.length s))]
      (if (zero? n)
        (print "0\n")
        (let [maxst (unchecked-multiply (int 2) (unchecked-inc n))
              nxt (java.util.ArrayList.)
              ^ints estart (int-array maxst)
              ^ints eend (int-array maxst)
              ^ints link (int-array maxst)
              ^booleans isleaf (boolean-array maxst)
              ^ints lend (int-array 1)
              ^ints depth (int-array maxst)
              ^ints parent (int-array maxst)
              ^ints cnt (int-array maxst)
              ^ints stack (int-array maxst)
              ^ints order (int-array maxst)]
          (.add nxt (int-array 27 -1))
          (aset link (int 0) (int -1))
          (aset parent (int 0) (int -1))
          (aset stack (int 0) (int 0))
          ;; Ukkonen over s[0..n-1] plus a terminal sentinel at index n
          ;; (child slot 26). Active point = (aN, aE, aL); all leaves share
          ;; the global open end lend. Single loop: fresh=1 runs the phase
          ;; init, rem=0 advances to the next phase, else one extension.
          (loop [i (int 0)
                 aN (int 0)
                 aE (int 0)
                 aL (int 0)
                 rem (int 0)
                 sz (int 1)
                 lastNew (int -1)
                 fresh (int 1)]
            (cond
              (> i n)
              nil
              ;; phase init: open the leaves to i, one more pending suffix
              (== fresh (int 1))
              (do
                (aset lend (int 0) i)
                (recur i aN aE aL (unchecked-inc rem) sz (int -1) (int 0)))
              ;; phase done: every pending suffix is extended
              (zero? rem)
              (recur (unchecked-inc i) aN aE aL rem sz lastNew (int 1))
              :else
              (let [aE (if (zero? aL) i aE)
                    ch (if (< i n)
                         (unchecked-subtract (int (.charAt s i)) (int 97))
                         (int 26))
                    c (if (< aE n)
                        (unchecked-subtract (int (.charAt s aE)) (int 97))
                        (int 26))
                    child (aget ^ints (.get nxt aN) c)]
                (if (neg? child)
                  ;; extension rule 2: append a new leaf under aN
                  (let [leaf sz]
                    (.add nxt (int-array 27 -1))
                    (aset estart leaf i)
                    (aset link leaf (int -1))
                    (aset isleaf leaf true)
                    (aset ^ints (.get nxt aN) c (int leaf))
                    (when-not (neg? lastNew)
                      (aset link lastNew aN))
                    (let [rem (unchecked-dec rem)]
                      (if (and (== aN (int 0)) (pos? aL))
                        (recur i aN
                               (unchecked-inc (unchecked-subtract i rem))
                               (unchecked-dec aL)
                               rem (unchecked-inc sz) (int -1) (int 0))
                        (if (== aN (int 0))
                          (recur i aN aE aL rem
                                 (unchecked-inc sz) (int -1) (int 0))
                          (recur i (aget link aN) aE aL rem
                                 (unchecked-inc sz) (int -1) (int 0))))))
                  ;; child edge exists: walk down, rule 3, or split
                  (let [elen (if (aget isleaf child)
                               (unchecked-inc (unchecked-subtract
                                               (aget lend (int 0))
                                               (aget estart child)))
                               (unchecked-inc (unchecked-subtract
                                               (aget eend child)
                                               (aget estart child))))]
                    (if (>= aL elen)
                      ;; walk down past child
                      (recur i child
                             (unchecked-add aE elen)
                             (unchecked-subtract aL elen)
                             rem sz lastNew (int 0))
                      (let [mid (unchecked-add (aget estart child) aL)
                            mc (if (< mid n)
                                 (unchecked-subtract
                                  (int (.charAt s mid)) (int 97))
                                 (int 26))]
                        (if (== mc ch)
                          ;; extension rule 3: already in the tree
                          (do
                            (when-not (neg? lastNew)
                              (aset link lastNew aN))
                            (recur (unchecked-inc i) aN aE
                                   (unchecked-inc aL)
                                   rem sz (int -1) (int 1)))
                          ;; extension rule 2: split the edge
                          (let [cs (aget estart child)
                                ncs (unchecked-add cs aL)
                                split sz
                                leaf (unchecked-inc sz)]
                            (.add nxt (int-array 27 -1))
                            (.add nxt (int-array 27 -1))
                            (aset estart split cs)
                            (aset eend split (unchecked-dec ncs))
                            (aset link split (int 0))
                            (aset estart leaf i)
                            (aset link leaf (int -1))
                            (aset isleaf leaf true)
                            (aset ^ints (.get nxt aN) c (int split))
                            (aset ^ints (.get nxt split) ch (int leaf))
                            (aset estart child ncs)
                            (aset ^ints (.get nxt split)
                                   (if (< ncs n)
                                     (unchecked-subtract
                                      (int (.charAt s ncs)) (int 97))
                                     (int 26))
                                   (int child))
                            (when-not (neg? lastNew)
                              (aset link lastNew split))
                            (let [rem (unchecked-dec rem)]
                              (if (and (== aN (int 0)) (pos? aL))
                                (recur i aN
                                       (unchecked-inc
                                        (unchecked-subtract i rem))
                                       (unchecked-dec aL)
                                       rem
                                       (unchecked-add sz (int 2))
                                       split (int 0))
                                (if (== aN (int 0))
                                  (recur i aN aE aL rem
                                         (unchecked-add sz (int 2))
                                         split (int 0))
                                  (recur i (aget link aN) aE aL rem
                                         (unchecked-add sz (int 2))
                                         split (int 0))))))))))))))
          ;; iterative DFS from the root: preorder, string depth (sentinel
          ;; excluded from leaf edges), and parents
          (let [norder (loop [top (int 0)
                               ordlen (int 0)
                               v (int 0)
                               cc (int 0)
                               fresh (int 1)]
                         ;; single loop (a nested loop's value would box):
                         ;; fresh=1 pops the next node, else scans child cc
                         (cond
                           (== fresh (int 1))
                           (if (neg? top)
                             ordlen
                             (let [v (aget stack top)]
                               (aset order ordlen v)
                               (recur (unchecked-dec top)
                                      (unchecked-inc ordlen)
                                      v (int 0) (int 0))))
                           (>= cc (int 27))
                           (recur top ordlen v cc (int 1))
                           :else
                           (let [w (aget ^ints (.get nxt v) cc)]
                             (if (neg? w)
                               (recur top ordlen v
                                      (unchecked-inc cc) fresh)
                               (do
                                 (aset depth w
                                       (unchecked-add
                                        (aget depth v)
                                        (if (aget isleaf w)
                                          (unchecked-subtract
                                           n (aget estart w))
                                          (unchecked-inc
                                           (unchecked-subtract
                                            (aget eend w)
                                            (aget estart w))))))
                                 (aset parent w v)
                                 (aset stack (unchecked-inc top) w)
                                 (recur (unchecked-inc top) ordlen v
                                        (unchecked-inc cc) fresh))))))]
            ;; bottom-up: leaf counts plus max of depth x count
            (loop [k (unchecked-dec norder) ans (long n)]
              (if (neg? k)
                (print (.concat (Long/toString ans) "\n"))
                (let [v (aget order k)]
                  (when (aget isleaf v)
                    (aset cnt v (int 1)))
                  (let [cand (unchecked-multiply (long (aget depth v))
                                                 (long (aget cnt v)))
                        ans (if (> cand ans) cand ans)
                        par (aget parent v)]
                    (when-not (neg? par)
                      (aset cnt par
                            (unchecked-add (aget cnt par) (aget cnt v))))
                    (recur (unchecked-dec k) ans)))))))))))
