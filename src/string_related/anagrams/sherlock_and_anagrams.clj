;; Sherlock and anagrams: count unordered substring pairs that are
;; anagrams of each other.
;; Input: a count q then q string tokens. Output: one count per line.
;; Lowercase strings use incremental 26 slot frequency signatures as
;; map keys, giving quadratic time in the string length, which is
;; optimal here since every substring start and end pair is visited
;; once. This replaces sorting every substring, which costs cubic
;; time times a log factor. Other chars fall back to incremental
;; sorted signatures with the same exact counts.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(def alpha-index
  "Lowercase chars mapped to their frequency slot."
  (zipmap "abcdefghijklmnopqrstuvwxyz" (range 26)))

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

(defn- lower-string?
  "True when every char of s is a lowercase letter."
  [s]
  (every? #(contains? alpha-index %) s))

(defn- pair-total
  "Sums n choose 2 over the signature frequencies in counts."
  [counts]
  (reduce (fn [tot c] (+' tot (quot (*' c (dec c)) 2))) 0 (vals counts)))

(defn- fast-pairs
  "Anagram pair count for a lowercase char vector via incremental
  frequency signatures."
  [cs]
  (let [n (count cs)
        zero (vec (repeat 26 0))]
    (pair-total
     (loop [start 0 counts {}]
       (if (>= start n)
         counts
         (recur (inc start)
                (loop [end start freq zero counts counts]
                  (if (>= end n)
                    counts
                    (let [freq (update freq (get alpha-index (nth cs end)) inc)]
                      (recur (inc end)
                             freq
                             (assoc counts freq (inc (get counts freq 0)))))))))))))

(defn- insert-sorted
  "Inserts char c into sorted char vector v, keeping it sorted."
  [v c]
  (let [n (count v)]
    (loop [i 0]
      (if (or (>= i n) (neg? (compare c (nth v i))))
        (into (conj (subvec v 0 i) c) (subvec v i))
        (recur (inc i))))))

(defn- general-pairs
  "Anagram pair count for any char vector via incremental sorted
  signatures."
  [cs]
  (let [n (count cs)]
    (pair-total
     (loop [start 0 counts {}]
       (if (>= start n)
         counts
         (recur (inc start)
                (loop [end start sig [] counts counts]
                  (if (>= end n)
                    counts
                    (let [sig (insert-sorted sig (nth cs end))]
                      (recur (inc end)
                             sig
                             (assoc counts sig (inc (get counts sig 0)))))))))))))

(defn- anagram-pairs
  "Anagram pair count for string s."
  [s]
  (let [cs (vec s)]
    (if (lower-string? cs)
      (fast-pairs cs)
      (general-pairs cs))))

(defn -main
  "Parses data and prints one anagram count per query string.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            q (parse-int (first toks) 0 2147483647)]
        (if (or (nil? q) (< (count toks) (inc' q)))
          (report "error: expected a count followed by that many strings")
          (let [out (map (comp str anagram-pairs) (take q (rest toks)))]
            (when (seq out)
              (print (str (str/join "\n" out) "\n"))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
