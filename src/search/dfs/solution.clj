;; Largest 8 connected land region in a binary grid.
;; Each land cell joins exactly one flood fill over eight neighbours, so the
;; scan takes O(n*m) time and space for the grid plus visit marks held in
;; persistent vectors. A blank region count of minus one marks a grid with
;; no land cells at all. Bad input shapes print a note on stderr and exit
;; zero, since a nonzero exit needs host calls, which this file avoids here.
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

(def nbr8
  "Row col steps for the eight neighbours of one cell."
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn flood-size
  "Flood fill from idx, returning [marks size] over grid dims n m."
  [grid n m seen idx]
  (loop [stack [idx] seen (assoc seen idx true) size 0]
    (if (empty? stack)
      [seen size]
      (let [cur (peek stack)
            cr (quot cur m)
            cc (rem cur m)
            [stack seen]
            (loop [ks nbr8 stack (pop stack) seen seen]
              (if (empty? ks)
                [stack seen]
                (let [[dr dc] (first ks)
                      nr (+ cr dr)
                      nc (+ cc dc)]
                  (if (and (<= 0 nr) (< nr n) (<= 0 nc) (< nc m))
                    (let [ni (+ (* nr m) nc)]
                      (if (and (= (nth grid ni) 1) (not (nth seen ni)))
                        (recur (rest ks) (conj stack ni) (assoc seen ni true))
                        (recur (rest ks) stack seen)))
                    (recur (rest ks) stack seen)))))]
        (recur stack seen (inc size))))))

(defn best-region
  "Largest 8 connected region of ones, minus one when no land exists."
  [grid n m]
  (let [nm (* n m)]
    (loop [idx 0 seen (vec (repeat nm false)) best -1]
      (if (>= idx nm)
        best
        (if (or (not= (nth grid idx) 1) (nth seen idx))
          (recur (inc idx) seen best)
          (let [[seen size] (flood-size grid n m seen idx)]
            (recur (inc idx) seen (max best size))))))))

(defn -main
  "Entry point reading the whole stdin string, silent on blank input."
  [input]
  (when-not (str/blank? input)
    (let [all (parse-all-ints input)]
      (if (nil? all)
        (fail "bad input: tokens must all be integers")
        (let [len (count all)]
          (if (< len 2)
            (fail "bad input: need dims then grid cells")
            (let [n (nth all 0) m (nth all 1) nm (*' n m)]
              (cond
                (or (<= n 0) (<= m 0))
                (if (zero? nm)
                  (println -1)
                  (fail "bad input: dims must stay positive"))
                (< len (+' 2 nm))
                (fail "bad input: grid cells missing")
                :else
                (println (best-region (subvec all 2 (+ 2 nm)) n m))))))))))

;; Minimal top level driver.
(-main (slurp *in*))
