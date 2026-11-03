;; No prefix set: a word is bad when it duplicates, prefixes, or is
;; prefixed by an earlier word; print GOOD SET, or BAD SET plus the
;; first bad word in input order.
;; Input: a count n then n word tokens.
;; Method: insert the words in input order into a persistent trie of
;; nested maps (:end marks a word end). A word is bad when its path
;; passes a node already marked :end (an earlier word prefixes it or
;; it duplicates one) or when its terminal node already has children
;; (it prefixes an earlier word). Each char costs one hash lookup
;; plus one assoc, so O(T) time for T total word chars and O(T)
;; nodes of space, matching the python twin. Pure clojure with no
;; host interop.
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

(defn- insert-word
  "Inserts word w into trie; returns [new-trie bad?]. bad? is true
   when an earlier word ends on w's path (prefix or duplicate of w)
   or w's terminal node already has children (w prefixes an earlier
   word). One pass down the path plus one bottom-up rebuild, each
   O(|w|) hash steps."
  [trie w]
  (let [chars (vec w)
        n (count chars)
        chain (loop [i 0 node trie ch (transient [trie])]
                (if (>= i n)
                  (persistent! ch)
                  (let [nx (get node (nth chars i) {})]
                    (recur (inc i) nx (conj! ch nx)))))
        leaf (nth chain n)
        bad (or (boolean (:end leaf))
                (boolean (seq (dissoc leaf :end)))
                (boolean (some :end (subvec chain 0 n))))
        rebuilt (loop [i (dec n) child (assoc leaf :end true)]
                  (if (neg? i)
                    child
                    (recur (dec i) (assoc (nth chain i) (nth chars i) child))))]
    [rebuilt bad]))

(defn- first-bad-word
  "First word of words, in input order, that duplicates, prefixes,
   or is prefixed by an earlier word; nil when the set is good.
   Words are inserted into a trie in input order and the first bad
   insertion is the answer: O(T) time for T total chars."
  [words]
  (loop [ws (seq words) trie {}]
    (when ws
      (let [[t bad] (insert-word trie (first ws))]
        (if bad (first ws) (recur (next ws) t))))))

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
