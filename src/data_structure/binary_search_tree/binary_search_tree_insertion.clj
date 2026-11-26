;; https://www.hackerrank.com/challenges/binary-search-tree-insertion/problem
;; bst-insertion: insert values (duplicates go left) and print preorder.
(require '[clojure.string :as str])

(defn insert-node [t v]
  (if (nil? t)
    [v nil nil]
    (let [[root l r] t]
      (if (<= v root)
        [root (insert-node l v) r]
        [root l (insert-node r v)]))))

(defn preorder [t]
  (when t
    (let [[root l r] t]
      (cons root (concat (preorder l) (preorder r))))))

(defn -main
  "Read n and values, build the BST, print preorder with trailing space."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))
        nums (mapv #(Long/parseLong %) toks)]
    (when (and (>= (count nums) 1) (>= (nth nums 0) 0))
      (let [n (nth nums 0)
            vals (take n (drop 1 nums))
            tree (reduce insert-node nil vals)]
        (print (str (str/join " " (preorder tree)) " "))))))

(-main (slurp *in*))
