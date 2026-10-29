;; Boyer Moore substring search with the bad char plus good
;; suffix rules, which together give linear worst case search.
;; Input: a count n then n pairs of <text> <pattern> tokens.
;; Output: YES or NO per pair, each on its own line.
;; Preprocessing costs linear time and space in the pattern length,
;; search costs linear time in the text length. This replaces a bad
;; char only scan whose worst case is quadratic in the two lengths.
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
  (vec (remove str/blank? (str/split s #"[ \t\n\r\f]+"))))

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

(defn- suffix-lengths
  "For char vector p of length m, entry i holds the length of the
  longest suffix of p open to i matching a suffix of all of p.
  Linear time with the standard border walk."
  [p m]
  (loop [i (- m 2) f 0 g (dec m) suf (assoc (vec (repeat m 0)) (dec m) m)]
    (if (neg? i)
      suf
      (if (and (> i g) (< (nth suf (+ i m -1 (- f))) (- i g)))
        (recur (dec i) f g (assoc suf i (nth suf (+ i m -1 (- f)))))
        (let [g (if (< i g) i g)
              f i
              g (loop [g g]
                  (if (and (>= g 0) (= (nth p g) (nth p (+ g m -1 (- f)))))
                    (recur (dec g))
                    g))]
          (recur (dec i) f g (assoc suf i (- f g))))))))

(defn- good-suffix-shifts
  "Good suffix shift table for char vector p of length m: entry i is
  how far to slide when position i mismatches. Linear time."
  [p m]
  (let [suf (suffix-lengths p m)
        base (vec (repeat m m))
        full (loop [i (dec m) j 0 tab base]
               (if (< i -1)
                 tab
                 (if (or (= i -1) (= (nth suf i) (inc i)))
                   (let [lim (- m 1 i)]
                     (recur (dec i)
                            lim
                            (reduce (fn [tb k]
                                      (if (= (nth tb k) m) (assoc tb k lim) tb))
                                    tab
                                    (range j lim))))
                   (recur (dec i) j tab))))]
    (reduce (fn [tb i] (assoc tb (- m 1 (nth suf i)) (- m 1 i)))
            full
            (range (- m 1)))))

(defn- bm-contains?
  "True when string pat occurs in string text. Strings are indexed
   directly (count/nth are O(1)) instead of copying to char vectors.
   The search is one loop (j = alignment, i = pattern index under
   comparison) whose numbers are all primitive loop vars -- boxed
   lets cannot hold primitives, so n/m are unboxed once into the
   loop. Only inlined primops (+ - < >) appear and the shift is
   recomputed in each if-branch rather than let-bound (a let slot
   would box once per alignment), so a comparison allocates nothing
   (verified under a non-collecting heap). No host hints are used."
  [text pat]
  (let [n0 (count text)
        m0 (count pat)]
    (cond
      (zero? m0) true
      (< n0 m0) false
      :else
      (let [last-at (reduce (fn [mp j] (assoc mp (nth pat j) j)) {} (range m0))
            gs (good-suffix-shifts pat m0)]
        (loop [j (long 0) i (long (dec m0)) n (long n0) m (long m0)]
          (if (> (+ j m) n)
            false
            (if (< i 0)
              true
              (if (= (nth pat i) (nth text (+ i j)))
                (recur j (- i 1) n m)
                (recur (+ j (if (> (long (nth gs i))
                                         (- i (long (get last-at (nth text (+ i j)) -1))))
                                      (long (nth gs i))
                                      (- i (long (get last-at (nth text (+ i j)) -1)))))
                       (- m 1) n m)))))))))
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
          (let [out (map (fn [[text pat]]
                           (if (bm-contains? text pat) "YES" "NO"))
                         (partition 2 (take (* 2 t) (rest toks))))]
            (when (seq out)
              (print (str (str/join "\n" out) "\n"))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
