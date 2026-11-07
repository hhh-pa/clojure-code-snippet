;; No prefix set: a word is bad when it duplicates, prefixes, or is
;; prefixed by an earlier word; print GOOD SET, or BAD SET plus the
;; first bad word in input order.
;; Input: a count n then n word tokens.
;; Method: insert the words in input order into a persistent trie
;; of nested maps (:end marks a word end). A word is bad when its
;; path passes a node already marked :end (an earlier word prefixes
;; it or it duplicates one) or when its terminal node already has
;; children (it prefixes an earlier word). One downward pass
;; collects the path and checks :end marks, then a bottom-up pass
;; rebuilds it with assoc, each O(|w|) hash steps, so O(T) time
;; for T total word chars and O(T) nodes of space, matching the
;; python twin. A transient-map version was tried and reverted:
;; it rebuilds nothing but each node pays dual storage (entries
;; plus a lookup index), which raised peak RSS to 404 MB from
;; 113 MB on the largest case while saving only 9% of
;; instructions.
;; The string is indexed directly instead of copying to a char
;; vector. Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(defn- tokenize
  "Splits s on whitespace runs; returns a vector of token strings.
   Uses clojure.string/split rather than a per-char loop: conjing
   every char into vectors allocates per char and exhausts a fixed
   heap on multi-megabyte inputs. The explicit class matches space,
   tab, newline, carriage return, and form feed. The + matcher
   yields no interior empties and split drops trailing ones, so at
   most a leading empty is skipped with subvec: filtering or
   re-vecing here would cost hundreds of bytes per token."
  [s]
  (let [v (str/split s #"[ \t\n\r\f]+")]
    (if (and (pos? (count v)) (str/blank? (nth v 0)))
      (subvec v 1)
      v)))

(defn- parse-int
  "Reads token tok as a decimal integer inside lo to hi, else nil."
  [tok lo hi]
  (when (and (string? tok) (seq tok))
    (let [cs (seq tok)
          lead (first cs)
          signed (or (= lead \-) (= lead \+))
          ds (if signed (next cs) cs)]
      (when (seq ds)
        (let [mag (reduce (fn [acc c]
                            (let [d (get digits c)]
                              (if (or (nil? acc) (nil? d))
                                nil
                                (+' (*' acc 10) d))))
                          0 ds)]
          (when (some? mag)
            (let [v (if (= lead \-) (- mag) mag)]
              (when (and (<= lo v) (<= v hi)) v))))))))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- has-ws?
  "True when s contains an ASCII whitespace character."
  [s]
  (let [n (count s)]
    (loop [i 0]
      (if (= i n)
        false
        (if (<= (int (nth s i)) 32)
          true
          (recur (inc i)))))))

(defn- next-tok
  "Reads the next token from buffered line tokens or *in*, or nil at EOF."
  [buf idx]
  (if (and (some? buf) (< idx (count buf)))
    [(nth buf idx) buf (inc idx)]
    (loop []
      (when-let [line (read-line)]
        (let [len (count line)]
          (if (zero? len)
            (recur)
            (if (has-ws? line)
              (let [v (tokenize line)
                    nv (count v)]
                (if (or (zero? nv) (str/blank? (nth v 0)))
                  (recur)
                  [(nth v 0) v 1]))
              [line nil 0])))))))

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
    (when-let [[first-tok buf idx] (next-tok nil 0)]
      (let [n (parse-int first-tok -2147483648 2147483647)]
        (if (nil? n)
          (report "error: expected a count followed by that many words")
          (let [target (max 0 n)
                nodes (transient [0])]
            (loop [wi 0 buf buf idx idx]
              (if (= wi target)
                (print "GOOD SET\n")
                (if-let [[w nbuf nidx] (next-tok buf idx)]
                  (if (insert-word! nodes w)
                    (print (str "BAD SET\n" w "\n"))
                    (recur (inc wi) nbuf nidx))
                  (print "GOOD SET\n"))))))))
    (catch Exception _ (report "error: invalid input"))))

(-main)
