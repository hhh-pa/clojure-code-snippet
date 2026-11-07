;; No prefix set: a word is bad when it duplicates, prefixes, or is
;; prefixed by an earlier word; print GOOD SET, or BAD SET plus the
;; first bad word in input order.
;; Input: a count n then n word tokens.
;; Method: sort the words (carrying input indices), then sweep with
;; an ancestor stack. Popping stack tops that do not prefix the
;; current word (equality counts) leaves exactly its prefix-related
;; predecessors in sorted order: any with a smaller input index
;; makes the current word bad, and the smallest larger stack index
;; is itself bad for prefixing this earlier word. The first bad
;; word is the minimum candidate over the sweep. Sorting dominates
;; at O(W log W) native comparisons for W words; per-character trie
;; walks in the interpreter cost several times more instructions.
;; Pure clojure with no host interop.
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


(defn- first-bad
  "Input index of the first bad word in words, or -1 when the set is good."
  [words]
  (let [pairs (vec (map-indexed (fn [i w] [w i]) words))
        spairs (vec (sort pairs))
        m (count spairs)]
    (loop [k 0 stack [] best -1]
      (if (= k m)
        best
        (let [w (nth (nth spairs k) 0)
              j (nth (nth spairs k) 1)
              stack (loop [st stack]
                      (if (and (seq st)
                               (not (str/starts-with? w (nth (peek st) 0))))
                        (recur (pop st))
                        st))
              cand (loop [si 0 cand -1]
                     (if (= si (count stack))
                       cand
                       (let [a (nth (nth stack si) 1)]
                         (cond
                           (< a j) j
                           (> a j) (recur (inc si)
                                          (if (or (< cand 0) (< a cand)) a cand))
                           :else (recur (inc si) cand)))))
              best (if (and (>= cand 0) (or (< best 0) (< cand best))) cand best)]
          (recur (inc k) (conj stack [w j]) best))))))

(defn -main
  "Reads all tokens and prints the set verdict.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  []
  (try
    (let [data (slurp *in*)]
      (when-not (str/blank? data)
        (let [toks (tokenize data)
              n (parse-int (first toks) -2147483648 2147483647)]
          (if (nil? n)
            (report "error: expected a count followed by that many words")
            (let [words (subvec toks 1 (min (count toks) (inc (max 0 n))))
                  bad (first-bad words)]
              (if (< bad 0)
                (print "GOOD SET\n")
                (print (str "BAD SET\n" (nth words bad) "\n"))))))))
    (catch Exception _ (report "error: invalid input"))))

(-main)
