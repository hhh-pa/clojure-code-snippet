;; No prefix set: a word is bad when it duplicates, prefixes, or is
;; prefixed by an earlier word; print GOOD SET, or BAD SET plus the
;; first bad word in input order.
;; Input: a count n then n word tokens.
;; Method: stable-sort the words, then find for each word the
;; least-indexed word it clashes with. Earlier clashes (prefixes and
;; earlier duplicates of the word) come from one left-to-right pass
;; with a monotone stack of active prefixes, each entry carrying the
;; minimum original index below it. Later clashes (words the word
;; prefixes) form one contiguous sorted block whose end is found by
;; binary search over a segment tree of adjacent LCP lengths, with a
;; second segment tree supplying the block minimum. The least bad
;; original index is the answer. Sorting dominates at O(T log n) time
;; for T total word chars; extra space is O(n) small words plus the
;; input tokens. A persistent trie would copy O(depth) nodes per word
;; and exhaust a fixed heap on large inputs; this version allocates
;; about one small object per word outside the tokens. Hot loops keep
;; every counter primitive and call only inlined primops, so a loop
;; step allocates nothing. Pure clojure with no host interop.
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

(defn- lcp-len
  "Length of the longest common prefix of strings a and b, whose
   lengths arrive as ^long params so no count boxes inside. The
   scan is primitive-only and allocates nothing; the result boxes
   once per call."
  [a b ^long la ^long lb]
  (loop [i (long 0)]
    (if (or (>= i la) (>= i lb) (not (= (nth a i) (nth b i))))
      i
      (recur (+ i 1)))))

(defn- seg-build
  "Builds an iterative segment tree (range minimum) over the n
   numbers in leaves; returns {:t tree :n n} with 2n slots. Leaves
   land through reduce/conj! (no per-index key box); only the n
   internal nodes pay one key box each."
  [leaves n]
  (let [t (reduce conj! (transient (vec (repeat n (nth leaves 0)))) leaves)]
    (loop [i (long (- n 1)) t t]
      (if (< i 1)
        {:t (persistent! t) :n n}
        (recur (- i 1)
               (assoc! t i (min (nth t (* 2 i)) (nth t (+ (* 2 i) 1)))))))))

(defn- seg-min
  "Minimum over seg on [l, r). Bounds are ^long params (primitive
   fns take at most 4 args, so the tree travels as one map); the
   loop stays primitive and min reuses tree entries, so a query
   allocates nothing. inf fills empty ranges, never queried here."
  [seg ^long l ^long r inf]
  (let [t (:t seg) n (long (:n seg))]
    (loop [l (long (+ l n)) r (long (+ r n)) best inf]
      (if (>= l r)
        best
        (let [bl (== (bit-and l 1) 1)
              br (== (bit-and r 1) 1)]
          (recur (bit-shift-right (if bl (+ l 1) l) 1)
                 (bit-shift-right (if br (- r 1) r) 1)
                 (min (if bl (min best (nth t l)) best)
                      (if br (min best (nth t (- r 1))) best))))))))

(defn- block-end
  "Largest R such that every LCP value on [L, R] is at least t,
   where L is k+1 and the LCP array lives in seg; k when A[L] is
   below t or the range is empty. Words the k-th sorted word
   prefixes occupy exactly (k..R]. Binary search with range-minimum
   probes: O(log^2 n) time, zero allocation (the midpoint is
   recomputed with inlined primops instead of let-bound, since a
   let slot would box once per step). infA exceeds every LCP value."
  [seg ^long k ^long t infA]
  (let [n (long (:n seg))]
    (loop [lo (long (+ k 1)) hi (long n) tv (long t)]
      (if (>= lo hi)
        (- lo 1)
        ;; mid is spelled out at each use: binding it would box once
        ;; per step, O(n log n) garbage over the whole pass.
        (let [q (seg-min seg lo (+ (+ lo (bit-shift-right (- hi lo) 1)) 1) infA)]
          (if (< (long q) tv)
            (recur lo (+ lo (bit-shift-right (- hi lo) 1)) tv)
            (recur (+ (+ lo (bit-shift-right (- hi lo) 1)) 1) hi tv)))))))

(defn- first-bad-word
  "First word of words, in input order, that duplicates, prefixes,
   or is prefixed by an earlier word; nil when the set is good.
   Words are stable-sorted (duplicates keep index order). Earlier
   clashes of each word are exactly the active prefix stack after
   popping entries longer than the LCP with the previous word, so
   the stack top's carried minimum is the least earlier clash in
   O(1). Later clashes occupy one sorted block found by block-end,
   minimized by a segment tree over original indices. The least
   bad original index is the answer."
  [words]
  (let [n (count words)]
    (when (pos? n)
      (let [sp (sort-by (fn [i] (nth words i)) (range n))
            orig (vec sp)
            word-at (fn [^long k] (nth words (nth orig k)))
            [A maxlen] (loop [k (long 1)
                              a (transient (vec (repeat n 0)))
                              la (long (count (word-at 0)))
                              ml (long 0)]
                         (if (>= k n)
                           [(persistent! a) ml]
                           (let [b (word-at k)
                                 lb (long (count b))
                                 l (lcp-len (word-at (- k 1)) b la lb)]
                             (recur (+ k 1)
                                    (assoc! a k l)
                                    lb
                                    (if (> lb ml) lb ml)))))
            segO (seg-build orig n)
            segA (seg-build A n)
            infA (+ (long maxlen) 1)
            lens (transient (vec (repeat n 0)))
            pmins (transient (vec (repeat n 0)))]
        ;; The prefix stack lives in two presized transients (word
        ;; length, carried index minimum) under one primitive depth:
        ;; push/pop are one assoc!/depth step with no nodes. Each
        ;; pass allocates O(1) small boxes per word outside the
        ;; tokens and the O(n) structures above.
        (loop [k (long 0) depth (long 0) best nil]
          (if (>= k n)
            (when (some? best) (nth words best))
            (let [w (word-at k)
                  i (nth orig k)
                  dnew (loop [d depth
                              lv (if (== k 0) (long 0) (long (nth A k)))]
                         (if (and (> d 0) (> (long (nth lens (- d 1))) lv))
                           (recur (- d 1) lv)
                           d))
                  m1 (if (> (long dnew) 0) (nth pmins (- (long dnew) 1)) n)
                  t (count w)
                  R (block-end segA k (long t) infA)
                  m2 (if (> (long R) k)
                       (seg-min segO (+ k 1) (+ (long R) 1) n)
                       n)]
              (assoc! lens dnew t)
              (assoc! pmins dnew (min m1 i))
              (recur (+ k 1)
                     (+ (long dnew) 1)
                     (if (and (or (< (long m1) (long i)) (< (long m2) (long i)))
                              (or (nil? best) (< (long i) (long best))))
                       i
                       best)))))))))

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
