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

(defn- find-child
  "Finds child of node u with char code c in the first-child/next-sibling
   arrays, or -1 when absent."
  [fc ns cc u c]
  (loop [v (nth fc u)]
    (cond
      (= v -1) -1
      (= (nth cc v) c) v
      :else (recur (nth ns v)))))

(defn- first-bad-word
  "First word of words, in input order, that duplicates, prefixes,
   or is prefixed by an earlier word; nil when the set is good.
   The trie is stored in four flat transient int vectors (first-child,
   next-sibling, char-code, is-end) so every node costs 4 unboxed longs
   with zero map allocations."
  [words]
  (let [fc (transient [-1])
        ns (transient [-1])
        cc (transient [0])
        en (transient [0])
        cnt (count words)]
    (loop [wi 0]
      (when (< wi cnt)
        (let [w (nth words wi)
              m (count w)]
          (if (zero? m)
            (if (or (= (nth en 0) 1) (not= (nth fc 0) -1))
              w
              (do (assoc! en 0 1)
                  (recur (inc wi))))
            (if (= (nth en 0) 1)
              w
              (let [bad? (loop [i 0 u 0]
                           (let [c (int (nth w i))
                                 v (find-child fc ns cc u c)]
                             (if (not= v -1)
                               (if (or (= (nth en v) 1) (= i (dec m)))
                                 true
                                 (recur (inc i) v))
                               (let [first-id (count fc)
                                     old-fc (nth fc u)
                                     last-i (dec m)]
                                 (assoc! fc u first-id)
                                 (loop [j i nid first-id]
                                   (let [last? (= j last-i)]
                                     (conj! fc (if last? -1 (inc nid)))
                                     (conj! ns (if (= j i) old-fc -1))
                                     (conj! cc (int (nth w j)))
                                     (conj! en (if last? 1 0))
                                     (when-not last?
                                       (recur (inc j) (inc nid)))))
                                 false))))]
                (if bad?
                  w
                  (recur (inc wi)))))))))))

(defn -main
  "Parses data and prints the set verdict.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            n (parse-int (first toks) -2147483648 2147483647)]
        (if (nil? n)
          (report "error: expected a count followed by that many words")
          (let [words (subvec toks 1 (min (count toks) (+ 1 (max 0 n))))
                bad (first-bad-word words)]
            (if (nil? bad)
              (print "GOOD SET\n")
              (print (str "BAD SET\n" bad "\n"))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
