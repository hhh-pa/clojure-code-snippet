;; Boyer Moore substring search with the bad character rule,
;; mirroring the python twin exactly: a 256-slot last-occurrence
;; table indexed by char code, plus a small overflow map for codes
;; past latin-1 (which the twin's 256-slot list cannot index, but
;; which must still search correctly here).
;; Input: a count n then n pairs of <text> <pattern> tokens.
;; Output: YES or NO per pair, each on its own line.
;; Preprocessing costs linear time and space in the pattern length.
;; Search is sublinear on average with a quadratic worst case, like
;; the twin. This replaces a bad char plus good suffix scan whose
;; table build cost more than the twin's whole search.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings.
   Uses clojure.string/split rather than a per-char loop: a 60MB text
   token would otherwise allocate gigabytes of transient vectors under
   a non-collecting heap. The explicit class matches space, tab,
   newline, return and formfeed exactly (no vertical tab, unlike \\s)."
  [s]
  (if (nil? s)
    []
    (let [v (str/split s #"[ \t\n\r\f]+")
          n (count v)
          start (if (and (pos? n) (str/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (str/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

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

(defn- bm-contains?
  "True when string pat occurs in string text. Bad character rule
   only, like the python twin: the last-occurrence table is a
   256-slot vector indexed by char code (built transiently, since
   patterns run long), with a map fallback for codes past latin-1.
   Strings are indexed directly (count/nth are O(1)) instead of
   copying to char vectors. The outer loop carries the alignment;
   the inner loop walks the pattern right to left and returns the
   mismatch index, or -1 when the pattern matches. Only inlined
   primops (+ - < >) appear in the loops. No host hints are used."
  [text pat]
  (let [n0 (count text)
        m0 (count pat)]
    (cond
      (zero? m0) true
      (< n0 m0) false
      :else
      (let [last-j (dec m0)
            last-p (nth pat last-j)]
        (if (zero? last-j)
          (loop [i 0]
            (if (= i n0)
              false
              (if (= (nth text i) last-p)
                true
                (recur (inc i)))))
          (let [right (transient (vec (repeat 256 -1)))
                big (loop [j 0 mp {}]
                      (if (= j m0)
                        mp
                        (let [ch (nth pat j)
                              c (int ch)]
                          (if (< c 256)
                            (do (assoc! right c j)
                                (recur (inc j) mp))
                            (recur (inc j) (assoc mp ch j))))))
                limit (- n0 m0)]
            (loop [i (long 0)]
              (if (> i limit)
                false
                (let [ch0 (nth text (+ i last-j))]
                  (if (not= ch0 last-p)
                    (let [c (int ch0)
                          r (if (< c 256) (nth right c) (get big ch0 -1))
                          skip (- last-j r)]
                      (recur (+ i (if (< skip 1) 1 skip))))
                    (let [j (loop [j (dec last-j)]
                              (cond (neg? j) -1
                                    (= (nth pat j) (nth text (+ i j))) (recur (dec j))
                                    :else j))]
                      (if (neg? j)
                        true
                        (let [ch (nth text (+ i j))
                              c (int ch)
                              r (if (< c 256) (nth right c) (get big ch -1))
                              skip (- j r)]
                          (recur (+ i (if (< skip 1) 1 skip))))))))))))))))
(defn -main
  "Parses data and prints YES or NO per query pair.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            t (parse-int (first toks) 0 2147483647)]
        (if (or (nil? t) (< (count toks) (inc' (*' 2 t))))
          (report "error: expected a count followed by that many text pattern pairs")
          (when (pos? t)
            (loop [k 0 idx 1 out (transient [])]
              (if (= k t)
                (print (str (str/join "\n" (persistent! out)) "\n"))
                (let [text (nth toks idx)
                      pat (nth toks (inc idx))]
                  (recur (inc k) (+ idx 2)
                         (conj! out (if (bm-contains? text pat) "YES" "NO")))))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
