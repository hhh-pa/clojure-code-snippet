;; Zeta function substring search.
;; Input: a count n then n pairs of <text> <pattern> tokens.
;; Output: YES or NO per pair, each on its own line.
;; Cost per pair is linear time and space in the text plus pattern
;; length, which is optimal here. Pure clojure with no host interop.
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
  (if (nil? s)
    []
    (let [v (clojure.string/split s #"[ \t\n\r\f]+")
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

(defn- z-contains?
  "True when pattern string pat occurs in text string text, found
  with the zeta function of pattern plus separator plus text."
  [text pat]
  (let [m (count pat)]
    (if (zero? m)
      true
      (let [b (str pat "$" text)
            n (count b)]
        (loop [i 1 l 0 r 0 z (transient (vec (repeat n 0)))]
          (if (>= i n)
            false
            (let [zi (if (<= i r) (min (- (inc r) i) (nth z (- i l))) 0)
                  zi (loop [zv zi]
                       (if (and (< (+ i zv) n) (= (nth b zv) (nth b (+ i zv))))
                         (recur (inc zv))
                         zv))]
              (assoc! z i zi)
              (if (= zi m)
                true
                (if (and (pos? (+ i zi)) (> (+ i zi -1) r))
                  (recur (inc i) i (+ i zi -1) z)
                  (recur (inc i) l r z))))))))))

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
                         (conj! out (if (z-contains? text pat) "YES" "NO")))))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
