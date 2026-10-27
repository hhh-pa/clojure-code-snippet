;; No-prefix-set: sequential trie insertion; a word is BAD if it is a prefix of,
;; is prefixed by, or duplicates an earlier word. Port of
;; cpp/src/string_related/trie/no_prefix_set.cc (+ Go mirror).
;; Trie edges live in flat int arrays (linked adjacency: head/to/ch/nxt).
;; Input: n then n word tokens. Output "GOOD SET" or "BAD SET\n<word>".
(require '[clojure.string :as str])

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          n (Integer/parseInt (first toks))
          words (vec (take n (rest toks)))
          total (reduce + 1 (map count words))
          ^ints head (int-array total -1)
          ^ints tos (int-array total)
          ^ints chs (int-array total)
          ^ints nxts (int-array total)
          ^bytes endf (byte-array total)
          ^bytes hasc (byte-array total)
          ecnt (int-array 1 0)]
      (loop [wi (int 0)]
        (if (>= wi (count words))
          (print "GOOD SET\n")
          (let [^String w (nth words wi)
                nw (.length w)
                bad (loop [i (int 0) cur (long 0) bad false]
                      (if (>= i nw)
                        (let [bad (or bad (pos? (aget hasc cur)))]
                          (aset endf cur (byte 1))
                          bad)
                        (let [c (int (.charAt w i))
                              child (loop [e (aget head cur)]
                                      (cond
                                        (neg? e)
                                        (let [id (aget ecnt 0)]
                                          (aset ecnt 0 (inc id))
                                          ;; every edge creates exactly one node;
                                          ;; node id = edge id + 1 (root is 0)
                                          (aset tos id (inc id))
                                          (aset chs id c)
                                          (aset nxts id (aget head cur))
                                          (aset head cur id)
                                          (aset hasc cur (byte 1))
                                          (inc id))
                                        (= (aget chs e) c) (aget tos e)
                                        :else (recur (aget nxts e))))]
                          (recur (inc i) (long child) (or bad (pos? (aget endf child)))))))]
            (if bad
              (print (.concat "BAD SET\n" (.concat w "\n")))
              (recur (inc wi)))))))))
