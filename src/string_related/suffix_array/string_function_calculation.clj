;; String function calculation: max over all distinct substrings of
;; length * occurrences. Port of
;; cpp/src/string_related/suffix_array/string_function_calculation.cc, using the
;; Go mirror's linear suffix-automaton formulation (the C++ suffix-array +
;; quadratic scan computes the same value but is too slow for n=1e5).
;; Input: single string token. Output: the maximum (64-bit).
(require '[clojure.string :as str])

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          ^String s (first toks)
          n (.length s)]
      (if (zero? n)
        (print "0\n")
        (let [maxst (* 2 n)
              nxt (java.util.ArrayList.)
              ^ints link (int-array maxst)
              ^ints len (int-array maxst)
              ^longs occ (long-array maxst)
              sz (int-array 1 1)]
          (.add nxt (int-array 26 -1))
          (aset link 0 -1)
          (loop [i (int 0) last (int 0)]
            (when (< i n)
              (let [c (- (int (.charAt s i)) 97)
                    cur (aget sz 0)]
                (aset sz 0 (inc cur))
                (aset len cur (inc (aget len last)))
                (aset link cur 0)
                (aset occ cur 1)
                (.add nxt (int-array 26 -1))
                (let [p (loop [p last]
                          (if (or (neg? p) (not (= (aget ^ints (.get nxt p) c) -1)))
                            p
                            (do
                              (aset ^ints (.get nxt p) c cur)
                              (recur (aget link p)))))]
                  (if (neg? p)
                    (aset link cur 0)
                    (let [q (aget ^ints (.get nxt p) c)]
                      (if (= (aget len p) (dec (aget len q)))
                        (aset link cur q)
                        (let [clone (aget sz 0)]
                          (aset sz 0 (inc clone))
                          (aset len clone (inc (aget len p)))
                          (aset link clone (aget link q))
                          (aset occ clone 0)
                          (.add nxt (aclone ^ints (.get nxt q)))
                          (loop [p p]
                            (when (and (>= p 0) (= (aget ^ints (.get nxt p) c) q))
                              (aset ^ints (.get nxt p) c clone)
                              (recur (aget link p))))
                          (aset link q clone)
                          (aset link cur clone))))))
                (recur (inc i) cur))))
          (let [nst (aget sz 0)
                ^ints cnt (int-array (inc n))]
            (dotimes [v nst]
              (let [l (aget len v)]
                (aset cnt l (inc (aget cnt l)))))
            (loop [i (int 1)]
              (when (<= i n)
                (aset cnt i (+ (aget cnt i) (aget cnt (dec i))))
                (recur (inc i))))
            (let [^ints order (int-array nst)]
              (loop [v (int (dec nst))]
                (when (>= v 0)
                  (let [l (aget len v)
                        nl (dec (aget cnt l))]
                    (aset cnt l nl)
                    (aset order nl v))
                  (recur (dec v))))
              (loop [v (int (dec nst))]
                (when (>= v 0)
                  (let [sv (aget order v)
                        lk (aget link sv)]
                    (when (>= lk 0)
                      (aset occ lk (+ (aget occ lk) (aget occ sv)))))
                  (recur (dec v))))
              (let [ans (loop [v (int 0) ans (long n)]
                          (if (>= v nst)
                            ans
                            (recur (inc v)
                                   (let [cand (* (long (aget len v)) (aget occ v))]
                                     (if (> cand ans) cand ans)))))]
                (print (.concat (Long/toString (long ans)) "\n"))))))))))
