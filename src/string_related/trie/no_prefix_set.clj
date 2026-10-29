;; No prefix set: sequential trie insertion where a word is bad when
;; it duplicates, prefixes, or is prefixed by an earlier word.
;; Input: a count n then n word tokens.
;; Output: GOOD SET, or BAD SET plus the first bad word.
;; Linear expected time and space in the total word length, which is
;; optimal here. Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(defn- blank-char?
  "True for the whitespace chars that split tokens."
  [c]
  (or (= c \space) (= c \tab) (= c \newline) (= c \return) (= c \formfeed)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [c (first cs)]
        (if (blank-char? c)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur c) toks))))))

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

(def empty-node
  "Trie node with no end mark and no child edges."
  {:end false :next {}})

(defn- insert-word
  "Inserts char seq cs into trie root. Returns the new root plus a
  flag that is true on a prefix conflict with an earlier word."
  [root cs]
  (loop [stack [] node root cs (seq cs) bad false]
    (if (nil? cs)
      (let [node-new (assoc node :end true)
            bad (or bad (boolean (seq (:next node))))]
        [(reduce (fn [child [parent c]]
                   (assoc parent :next (assoc (:next parent) c child)))
                 node-new
                 (rseq stack))
         bad])
      (let [c (first cs)
            child (get (:next node) c empty-node)]
        (recur (conj stack [node c]) child (next cs) (or bad (:end child)))))))

(defn- check-words
  "Returns the first bad word in words, or nil when all are good."
  [words]
  (loop [ws (seq words) trie empty-node]
    (when ws
      (let [[trie bad] (insert-word trie (first ws))]
        (if bad
          (first ws)
          (recur (next ws) trie))))))

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
          (let [words (vec (take (max 0 n) (rest toks)))
                bad (check-words words)]
            (if (nil? bad)
              (print "GOOD SET\n")
              (print (str "BAD SET\n" bad "\n"))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
