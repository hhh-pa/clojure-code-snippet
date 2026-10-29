;; Longest common subsequence length for two input lines.
;; Input: two lines of text; a missing second line counts as empty.
;; Output: the length on one line.
;; Classic two row dynamic program in O(n m) time and O(min(n m))
;; space, which is optimal for length only scoring. The shorter line
;; is kept as the row so the space bound always uses the minimum.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- lines-of
  "Splits s into lines, tolerating unix and windows line endings."
  [s]
  (loop [cs (seq s) cur [] lines []]
    (if (nil? cs)
      (if (or (seq cur) (empty? lines))
        (conj lines (apply str cur))
        lines)
      (let [c (first cs)]
        (cond
          (= c \newline)
          (recur (next cs) [] (conj lines (apply str cur)))
          (= c \return)
          (let [rest (next cs)]
            (recur (if (= (first rest) \newline) (next rest) rest)
                   []
                   (conj lines (apply str cur))))
          :else
          (recur (next cs) (conj cur c) lines))))))

(defn- lcs-length
  "Length of the longest common subsequence of char vectors a and b."
  [a b]
  (let [[xs ys] (if (>= (count a) (count b)) [a b] [b a])
        m (count ys)]
    (if (zero? m)
      0
      (loop [i 0 prev (transient (vec (repeat (inc m) 0)))]
        (if (>= i (count xs))
          (nth prev m)
          (let [cx (nth xs i)
                curr (loop [j 0 curr (transient (vec (repeat (inc m) 0)))]
                       (if (>= j m)
                         curr
                         (do
                           (assoc! curr (inc j)
                                   (if (= cx (nth ys j))
                                     (inc (nth prev j))
                                     (max (nth prev (inc j)) (nth curr j))))
                           (recur (inc j) curr))))]
            (recur (inc i) curr)))))))

(defn -main
  "Parses data and prints the common subsequence length.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [lines (lines-of data)
            a (vec (or (first lines) ""))
            b (vec (or (second lines) ""))]
        (println (lcs-length a b)))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
