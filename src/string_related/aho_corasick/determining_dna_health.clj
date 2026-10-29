;; Determining dna health with the Aho Corasick automaton.
;; Input: gene count, genes, health values, strand count, then one
;; first plus last plus strand triple per strand.
;; Output: min and max total health over the strands, one line.
;; Trie build plus failure links cost linear expected time in the
;; total gene length; each strand scans in linear time plus total
;; matches, which is optimal here. Matches inside the id range add
;; their gene health to the strand total.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(def alpha-index
  "Lowercase chars mapped to their slot, used for validation."
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
  (and (seq s) (every? #(contains? alpha-index %) s)))

(def empty-state
  "Automaton state with no edges, root fail link and no output."
  {:next {} :fail 0 :dict 0 :out []})

(defn- trie-insert
  "Inserts gene string with id gid into trie states."
  [states gene gid]
  (let [[states u]
        (reduce (fn [[st u] c]
                  (if-let [nx (get (:next (nth st u)) c)]
                    [st nx]
                    (let [nx (count st)
                          node (nth st u)]
                      [(assoc (conj st empty-state)
                              u
                              (assoc node :next (assoc (:next node) c nx)))
                       nx])))
                [states 0]
                gene)]
    (update-in states [u :out] conj gid)))

(defn- build-automaton
  "Trie of genes plus failure and output links, breadth first."
  [genes]
  (let [trie (reduce (fn [st [gid gene]] (trie-insert st gene gid))
                     [empty-state]
                     (map-indexed vector genes))]
    (loop [states trie
           q (vec (vals (:next (first trie))))
           qi 0]
      (if (>= qi (count q))
        states
        (let [u (nth q qi)
              [states q]
              (reduce (fn [[st qu] [c v]]
                        (let [f (loop [cur (:fail (nth st u))]
                                  (if (and (> cur 0)
                                           (not (contains? (:next (nth st cur)) c)))
                                    (recur (:fail (nth st cur)))
                                    cur))
                              vf (get (:next (nth st f)) c 0)
                              st (-> st
                                     (assoc-in [v :fail] vf)
                                     (assoc-in [v :dict]
                                               (if (seq (:out (nth st vf)))
                                                 vf
                                                 (:dict (nth st vf)))))]
                          [st (conj qu v)]))
                      [states q]
                      (:next (nth states u)))]
          (recur states q (inc qi)))))))

(defn- strand-health
  "Total health of strand inside first-id to last-id."
  [states health first-id last-id strand]
  (loop [cs (seq strand) u 0 acc 0]
    (if (nil? cs)
      acc
      (let [c (first cs)
            base (loop [cur u]
                   (if (and (> cur 0)
                            (not (contains? (:next (nth states cur)) c)))
                     (recur (:fail (nth states cur)))
                     cur))
            nu (get (:next (nth states base)) c 0)
            add (loop [curr nu add 0]
                  (if (> curr 0)
                    (recur (:dict (nth states curr))
                           (reduce (fn [a gid]
                                     (if (and (<= first-id gid) (<= gid last-id))
                                       (+' a (nth health gid))
                                       a))
                                   add
                                   (:out (nth states curr))))
                    add))]
        (recur (next cs) nu (+' acc add))))))

(defn- summarize
  "Min and max strand health over the flat id triples, or nil when
  a triple is malformed."
  [states health triples strands]
  (loop [k 0 lo -1 hi -1]
    (if (>= k strands)
      [lo hi]
      (let [first-id (parse-int (nth triples (* k 3)) -2147483648 2147483647)
            last-id (parse-int (nth triples (+ 1 (* k 3))) -2147483648 2147483647)
            strand (nth triples (+ 2 (* k 3)))]
        (if (or (nil? first-id) (nil? last-id) (not (lower-string? strand)))
          nil
          (let [tot (strand-health states health first-id last-id strand)]
            (recur (inc k)
                   (if (or (= lo -1) (< tot lo)) tot lo)
                   (if (or (= hi -1) (> tot hi)) tot hi))))))))

(defn -main
  "Parses data and prints the min and max strand health.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (vec (tokenize data))
            n (parse-int (first toks) 0 2147483647)
            ok-shape (and (some? n) (<= (+ 2 (* 2 n)) (count toks)))]
        (if (not ok-shape)
          (report "error: expected a gene count plus genes and health values")
          (let [genes (subvec toks 1 (inc n))
                hvals (map #(parse-int % -2147483648 2147483647)
                           (subvec toks (inc n) (+ 1 (* 2 n))))
                s (parse-int (nth toks (+ 1 (* 2 n))) -2147483648 2147483647)]
            (if (or (not (every? lower-string? genes))
                     (some nil? hvals)
                     (nil? s))
              (report "error: expected lowercase genes plus integer health values")
              (let [strands (max 0 s)
                    triples (subvec toks (+ 2 (* 2 n)))]
                (if (< (count triples) (*' 3 strands))
                  (report "error: expected one id range plus strand per query")
                  (let [res (summarize (build-automaton genes)
                                       (vec hvals) triples strands)]
                    (if (nil? res)
                      (report "error: expected integer ranges plus lowercase strands")
                      (let [[lo hi] res]
                        (println (str lo " " hi)))))))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
