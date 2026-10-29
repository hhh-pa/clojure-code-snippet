;; Manacher longest palindromic substring plus palindrome count.
;; Input: a single string token.
;; Output: max length, longest palindrome, total count, one per line.
;; Linear time and space in the string length, which is optimal here.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

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

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- manacher
  "Returns max length, longest palindrome and total palindrome
  count for string s with one linear scan of radii."
  [s]
  (let [t (vec (concat [\#] (interpose \# (seq s)) [\#]))
        m (count t)
        p (loop [i 1 c 0 r 0 p (transient (vec (repeat m 0)))]
            (if (>= i (dec m))
              (persistent! p)
              (let [mirror (- (* 2 c) i)
                    init (if (< i r) (min (- r i) (nth p mirror)) 0)
                    rad (loop [rad init]
                          (let [l (- i 1 rad)
                                rr (+ i 1 rad)]
                            (if (and (>= l 0) (< rr m) (= (nth t l) (nth t rr)))
                              (recur (inc rad))
                              rad)))]
                (assoc! p i rad)
                (if (> (+ i rad) r)
                  (recur (inc i) i (+ i rad) p)
                  (recur (inc i) c r p)))))
        [max-len center total]
        (loop [i 1 max-len 0 center 0 total 0]
          (if (>= i (dec m))
            [max-len center total]
            (let [pi (nth p i)
                  [max-len center] (if (> pi max-len) [pi i] [max-len center])]
              (recur (inc i) max-len center (+ total (quot (+ pi 1) 2))))))
        start (quot (- center max-len) 2)]
    [max-len (subs s start (+ start max-len)) total]))

(defn -main
  "Parses data and prints the palindrome summary lines.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)]
        (if (empty? toks)
          (report "error: expected a single string token")
          (let [[max-len longest total] (manacher (first toks))]
            (println max-len)
            (println longest)
            (println total))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
