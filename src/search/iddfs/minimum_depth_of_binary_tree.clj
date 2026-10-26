;; Faithful port of cpp/src/search/iddfs/minimum_depth_of_binary_tree.cc.
;; NOTE: the .cc builds its tree through a queue whose front entry is never
;; mutated (tie copies the pair), so every node i>=1 overwrites root->left;
;; the final tree is root(vals[0]) with left child node(vals[n-1]) only
;; (verified against the compiled binary on custom inputs). minDepth is then
;; computed by iterative-deepening DFS, mirrored below.
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
          n (if (pos? len) (aget all 0) 0)
          has-root (and (> n 0) (> len 1) (not= (aget all 1) -1))
          has-left (and (>= n 2) (> len n) (not= (aget all n) -1))]
      (letfn [(dls [is-root ^long depth]
                (if (<= depth 0)
                  false
                  (let [leaf (if is-root (not has-left) true)]
                    (if leaf
                      true
                      (dls false (dec depth))))))]
        (print (str (if (not has-root)
                      0
                      (loop [d (long 1)]
                        (if (dls true d) d (recur (inc d)))))
                    "\n"))))))
