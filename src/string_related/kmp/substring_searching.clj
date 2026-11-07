;; KMP substring search with the prefix function.
;; Input: a count n then n pairs of <text> <pattern> tokens.
;; Output: YES or NO per pair, each on its own line.
;; Cost per pair is linear time in the text plus pattern length and
;; linear space in the pattern length, which is optimal here.
;; Pure clojure with no host interop.
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

(defn- prefix-table
  "Prefix function of pat in linear time: entry i holds
  the length of the longest proper border of pat open to i."
  [pat m]
  (let [pi (transient (vec (repeat m 0)))
        p0 (nth pat 0)]
    (loop [i 1 j 0]
      (if (>= i m)
        pi
        (let [ch (nth pat i)
              j (if (zero? j)
                  (if (= ch p0) 1 0)
                  (let [j (loop [j j]
                            (if (and (pos? j) (not= ch (nth pat j)))
                              (recur (nth pi (dec j)))
                              j))]
                    (if (= ch (nth pat j)) (inc j) 0)))]
          (assoc! pi i j)
          (recur (inc i) j))))))

(defn- kmp-contains?
  "True when pat occurs in text."
  [text pat]
  (let [m (count pat)]
    (if (zero? m)
      true
      (let [n (count text)]
        (if (< n m)
          false
          (let [p0 (nth pat 0)]
            (if (= m 1)
              (loop [i 0]
                (if (= i n)
                  false
                  (if (= (nth text i) p0)
                    true
                    (recur (inc i)))))
              (let [pi (prefix-table pat m)]
                (loop [i 0 j 0]
                  (if (>= i n)
                    false
                    (let [ch (nth text i)]
                      (if (zero? j)
                        (if (= ch p0)
                          (recur (inc i) 1)
                          (recur (inc i) 0))
                        (let [j (loop [j j]
                                  (if (and (pos? j) (not= ch (nth pat j)))
                                    (recur (nth pi (dec j)))
                                    j))
                              j (if (= ch (nth pat j)) (inc j) 0)]
                          (if (= j m)
                            true
                            (recur (inc i) j)))))))))))))))

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
                         (conj! out (if (kmp-contains? text pat) "YES" "NO")))))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
