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

(defn- prefix-table
  "Prefix function of char vector pat in linear time: entry i holds
  the length of the longest proper border of pat open to i."
  [pat]
  (let [m (count pat)]
    (if (zero? m)
      []
      (loop [i 1 j 0 pi (transient (vec (repeat m 0)))]
        (if (>= i m)
          (persistent! pi)
          (let [j (loop [j j]
                    (if (and (pos? j) (not= (nth pat i) (nth pat j)))
                      (recur (nth pi (dec j)))
                      j))
                j (if (= (nth pat i) (nth pat j)) (inc j) j)]
            (assoc! pi i j)
            (recur (inc i) j pi)))))))

(defn- kmp-contains?
  "True when char vector pat occurs in char vector text."
  [text pat]
  (let [m (count pat)]
    (if (zero? m)
      true
      (let [n (count text)
            pi (prefix-table pat)]
        (loop [i 0 j 0]
          (if (>= i n)
            false
            (let [j (loop [j j]
                      (if (and (pos? j) (not= (nth text i) (nth pat j)))
                        (recur (nth pi (dec j)))
                        j))
                  j (if (= (nth text i) (nth pat j)) (inc j) j)]
              (if (= j m)
                true
                (recur (inc i) j)))))))))

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
                           (if (kmp-contains? (vec text) (vec pat)) "YES" "NO"))
                         (partition 2 (take (* 2 t) (rest toks))))]
            (when (seq out)
              (print (str (str/join "\n" out) "\n"))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
