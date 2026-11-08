;; No prefix set: a word is bad when it duplicates, prefixes, or is
;; prefixed by an earlier word; print GOOD SET, or BAD SET plus the
;; first bad word in input order.
;; Input: a count n then n word tokens.
;; Method: insert the words in input order into a packed trie of
;; 64-bit integer nodes with zero-allocation traversal.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(defn- tokenize
  "Splits s on whitespace runs; returns a vector of token strings."
  [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- insert-word!
  "Inserts w into packed trie nodes; returns true if w duplicates, prefixes,
   or is prefixed by an earlier word."
  [nodes w]
  (let [m (count w)
        root0 (nth nodes 0)]
    (if (zero? m)
      (if (or (pos? (bit-shift-right root0 56)) (pos? (bit-and root0 16777215)))
        true
        (do (assoc! nodes 0 (bit-or root0 72057594037927936))
            false))
      (if (pos? (bit-shift-right root0 56))
        true
        (let [last-i (dec m)]
          (loop [i 0 u 0]
            (let [c (int (nth w i))
                  v (loop [v (bit-and (nth nodes u) 16777215)]
                      (if (zero? v)
                        0
                        (let [nv (nth nodes v)]
                          (if (= (bit-and (bit-shift-right nv 48) 255) c)
                            v
                            (recur (bit-and (bit-shift-right nv 24) 16777215))))))]
              (if (pos? v)
                (if (or (pos? (bit-shift-right (nth nodes v) 56)) (= i last-i))
                  true
                  (recur (inc i) v))
                (let [first-id (count nodes)
                      u-val (nth nodes u)
                      old-fc (bit-and u-val 16777215)]
                  (assoc! nodes u (bit-or (bit-and u-val -16777216) first-id))
                  (loop [j i nid first-id]
                    (let [last? (= j last-i)]
                      (conj! nodes (bit-or (if last? 0 (inc nid))
                                           (if (= j i) (bit-shift-left old-fc 24) 0)
                                           (bit-shift-left (int (nth w j)) 48)
                                           (if last? 72057594037927936 0)))
                      (when-not last?
                        (recur (inc j) (inc nid)))))
                  false)))))))))

(defn -main
  "Streams tokens from *in* and prints the set verdict.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [n (parse-long (nth toks 0))]
              (if (or (nil? n) (neg? n))
                (report "error: expected a count followed by that many words")
                (let [nodes (transient [0])
                      limit (min (inc n) nt)]
                  (loop [i 1]
                    (if (= i limit)
                      (println "GOOD SET")
                      (let [w (nth toks i)]
                        (if (insert-word! nodes w)
                          (do (println "BAD SET") (println w))
                          (recur (inc i)))))))))))))
    (catch Exception _ (report "error: invalid input"))))

(-main)
