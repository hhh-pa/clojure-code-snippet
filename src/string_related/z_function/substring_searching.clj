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
  with the zeta function over pat and a streaming scan of text."
  [text pat]
  (let [m (count pat)]
    (if (zero? m)
      true
      (let [nt (count text)]
        (if (< nt m)
          false
          (let [p0 (nth pat 0)]
            (if (= m 1)
              (loop [i 0]
                (if (= i nt)
                  false
                  (if (= (nth text i) p0)
                    true
                    (recur (inc i)))))
              (let [z (transient (vec (repeat m 0)))]
                (loop [i 1 l 0 r 0]
                  (when (< i m)
                    (let [zi (if (<= i r)
                               (let [rem (- (inc r) i)
                                     zk (nth z (- i l))]
                                 (if (< rem zk) rem zk))
                               0)
                          zi (loop [zv zi]
                               (if (and (< (+ i zv) m) (= (nth pat zv) (nth pat (+ i zv))))
                                 (recur (inc zv))
                                 zv))
                          nr (dec (+ i zi))]
                      (assoc! z i zi)
                      (if (> nr r)
                        (recur (inc i) i nr)
                        (recur (inc i) l r)))))
                (loop [i 0 l 0 r -1]
                  (if (>= i nt)
                    false
                    (if (> i r)
                      (if (not= (nth text i) p0)
                        (recur (inc i) l r)
                        (let [zi (loop [zv 1]
                                   (if (and (< zv m) (< (+ i zv) nt) (= (nth pat zv) (nth text (+ i zv))))
                                     (recur (inc zv))
                                     zv))]
                          (if (= zi m)
                            true
                            (recur (inc i) i (dec (+ i zi))))))
                      (let [rem (- (inc r) i)
                            zk (nth z (- i l))
                            zi0 (if (< rem zk) rem zk)
                            zi (if (< zi0 rem)
                                 zi0
                                 (loop [zv zi0]
                                   (if (and (< zv m) (< (+ i zv) nt) (= (nth pat zv) (nth text (+ i zv))))
                                     (recur (inc zv))
                                     zv)))]
                        (if (= zi m)
                          true
                          (let [nr (dec (+ i zi))]
                            (if (> nr r)
                              (recur (inc i) i nr)
                              (recur (inc i) l r))))))))))))))))

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
