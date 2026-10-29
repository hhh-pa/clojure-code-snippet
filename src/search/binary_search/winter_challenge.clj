;; Range search for a key in sorted ints with binary search bounds.
;; Lower and upper bound scans halve the range each step, taking O(log n)
;; time and O(1) extra space, matching the reference port bound logic,
;; including its loop exit value of the last mid seen on a miss path.
;; Bad shapes that the reference prints as minus one keep that stdout form,
;; while bad tokens print a note on stderr and exit zero, since a nonzero
;; exit needs host calls, which this file avoids on purpose.
(require '[clojure.string :as str])

(def digit-values
  "Digit chars mapped to their numeric values for the pure number reader."
  (zipmap "0123456789" (range 10)))

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn fit-long
  "Returns n when it fits in a signed 64-bit long, else nil."
  [n]
  (when (<= min-long n max-long) n))


(defn whitespace?
  "True for the four blank chars treated as token separators."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

(defn tokenize
  "Split string s on blanks into a vector of token strings in linear time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (whitespace? ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-int-token
  "Read one signed integer token into a long, else nil for a bad token."
  [tok]
  (let [cs (seq tok)]
    (when (seq cs)
      (let [f (first cs)
            neg? (= f \-)
            ds (if (or neg? (= f \+)) (next cs) cs)]
        (when (seq ds)
          (loop [ds ds acc 0]
            (if (nil? ds)
              (fit-long (if neg? (-' acc) acc))
              (let [ch (first ds)]
                (if (contains? digit-values ch)
                  (recur (next ds) (+' (*' acc 10) (get digit-values ch)))
                  nil)))))))))

(defn parse-all-ints
  "Read every token of s as a signed integer, else nil for a bad token."
  [s]
  (let [toks (tokenize s)]
    (loop [ts (seq toks) out []]
      (if (nil? ts)
        out
        (let [v (parse-int-token (first ts))]
          (if (nil? v)
            nil
            (recur (next ts) (conj out v))))))))

(defn fail
  "Report a bad input shape on stderr and return nil with exit code zero."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn lower-bound
  "First index holding key, mirroring the reference miss behaviour."
  [arr n k]
  (loop [low 0 high (dec n) mid 0]
    (if (> low high)
      mid
      (let [mid2 (quot (+ low high) 2)
            v (nth arr mid2)]
        (cond
          (and (>= v k) (or (zero? mid2) (< (nth arr (dec mid2)) k))) mid2
          (>= v k) (recur low (dec mid2) mid2)
          :else (recur (inc mid2) high mid2))))))

(defn upper-bound
  "First index past key, mirroring the reference miss behaviour."
  [arr n k]
  (loop [low 0 high (dec n) mid 0]
    (if (> low high)
      mid
      (let [mid2 (quot (+ low high) 2)
            v (nth arr mid2)]
        (cond
          (and (> v k) (or (zero? mid2) (<= (nth arr (dec mid2)) k))) mid2
          (> v k) (recur low (dec mid2) mid2)
          :else (recur (inc mid2) high mid2))))))

(defn -main
  "Entry point reading the whole stdin string, silent on blank input."
  [input]
  (when-not (str/blank? input)
    (let [all (parse-all-ints input)]
      (if (nil? all)
        (fail "bad input: tokens must all be integers")
        (let [len (count all)]
          (when (pos? len)
            (let [n (nth all 0)]
              (if (or (<= n 0) (> (+' 2 n) len))
                (println "-1 -1 0")
                (let [arr (subvec all 1 (+ 1 n))
                      k (nth all (+ 1 n))
                      li (lower-bound arr n k)
                      ui (upper-bound arr n k)]
                  (if (not= (nth arr li) k)
                    (println "-1 -1 0")
                    (println (str li " " (dec ui) " " (- ui li)))))))))))))

;; Minimal top level driver.
(-main (slurp *in*))
