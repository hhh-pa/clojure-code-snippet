;; Shift-And / Baeza-Yates-Gonnet Bitap K-Mismatch Matcher (Gusfield Ch 4.2-4.3).
;; Input: Q followed by Q triples of <K> <pattern> <text> (0 <= K <= 5, m <= 60).
;; Output: space-separated 0-based starting indices with <= K mismatches, or -1.
;; Time complexity: O(m + |Sigma| + K * n) word operations per query; Space: O(|Sigma| + K).
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(defn- blank-char?
  "True for whitespace chars that split tokens."
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

(defn- build-char-masks
  "Maps each char in pat to its bitmask of pattern positions."
  [pat]
  (reduce-kv (fn [acc i ch]
               (update acc ch (fnil #(bit-or % (bit-shift-left 1 i)) 0)))
             {}
             pat))

(defn- bitap-k-mismatch
  "Returns vector of 0-based start indices in text matching pat within k mismatches."
  [k pat text]
  (let [m (count pat)
        n (count text)]
    (if (or (zero? m) (> m n))
      []
      (let [mask-all (dec (bit-shift-left 1 m))
            cmasks (build-char-masks pat)
            init-r (vec (repeat (inc k) 0))]
        (loop [j 0 r init-r matches []]
          (if (>= j n)
            matches
            (let [bc (get cmasks (nth text j) 0)
                  s0 (bit-and (bit-or (bit-shift-left (nth r 0) 1) 1) mask-all)
                  r0 (bit-and s0 bc)
                  [next-r _] (reduce (fn [[acc prev-s] d]
                                       (let [sd (bit-and (bit-or (bit-shift-left (nth r d) 1) 1) mask-all)
                                             rd (bit-or (bit-and sd bc) prev-s)]
                                         [(conj acc rd) sd]))
                                     [[r0] s0]
                                     (range 1 (inc k)))
                  matched? (and (>= (inc j) m) (bit-test (nth next-r k) (dec m)))
                  matches (if matched? (conj matches (- (inc j) m)) matches)]
              (recur (inc j) next-r matches))))))))

(defn -main
  "Parses input and runs Shift-And Bitap K-mismatch matcher on each query."
  [data]
  (when-not (str/blank? data)
    (let [toks (tokenize data)
          q (parse-int (first toks) 0 2147483647)]
      (if (or (nil? q) (< (count toks) (inc' (*' 3 q))))
        (report "error: expected Q followed by Q triples of K, pattern, text")
        (let [triples (partition 3 (take (* 3 q) (rest toks)))
              lines (loop [ts (seq triples) out []]
                      (if (nil? ts)
                        out
                        (let [[ktok pat text] (first ts)
                              k (parse-int ktok 0 60)]
                          (if (nil? k)
                            (do (report "error: invalid K") nil)
                            (let [res (bitap-k-mismatch k (vec pat) (vec text))
                                  line (if (empty? res) "-1" (str/join " " res))]
                              (recur (next ts) (conj out line)))))))]
          (when (seq lines)
            (print (str (str/join "\n" lines) "\n"))))))))

(let [data (slurp *in*)] (-main data))
