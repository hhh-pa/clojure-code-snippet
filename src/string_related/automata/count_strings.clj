;; Count strings of length l matching a regex over a and b symbols.
;; Input: a count t then t pairs of <regex> <l> tokens.
;; Output: one answer per line, modulo 1000000007.
;; Pipeline is regex to Thompson NFA to subset DFA to adjacency
;; matrix power, which is the practical optimum for exact counting:
;; matrix products cost cubic time in the DFA size times log l.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def MOD 1000000007)

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

(defn- attach
  "Appends node to kids, folding into an open union when present."
  [kids node]
  (let [n (count kids)]
    (if (and (pos? n)
             (let [b (peek kids)]
               (and (= (:op b) :union) (= (count (:kids b)) 1))))
      (assoc kids (dec n) (update (peek kids) :kids conj node))
      (conj kids node))))

(defn- parse-regex
  "Parses regex string r into nodes with op, ch and kids fields."
  [r]
  (let [cs (vec r)
        n (count cs)]
    (loop [i 0 root [] stack []]
      (if (>= i n)
        {:op :concat :ch nil :kids root}
        (let [c (nth cs i)]
          (cond
            (= c \()
            (recur (inc i) root (conj stack []))

            (= c \))
            (let [cur {:op :concat :ch nil :kids (peek stack)}
                  stack (pop stack)]
              (if (seq stack)
                (recur (inc i) root
                       (assoc stack (dec (count stack)) (attach (peek stack) cur)))
                (recur (inc i) (attach root cur) stack)))

            :else
            (let [in-par (boolean (seq stack))
                  kids (if in-par (peek stack) root)
                  nk (cond
                       (= c \*)
                       (assoc kids (dec (count kids))
                              (assoc (peek kids) :op :closure))
                       (= c \|)
                       (assoc kids (dec (count kids))
                              {:op :union :ch nil :kids [(peek kids)]})
                       :else
                       (attach kids {:op :concat :ch c :kids []}))]
              (if in-par
                (recur (inc i) root (assoc stack (dec (count stack)) nk))
                (recur (inc i) nk stack)))))))))

(defn- build-nfa
  "Thompson NFA for parsed root. Returns adjacency plus in and out
  states. Edge labels are 0 for empty, 1 for a, 2 for b."
  [root]
  (letfn [(fresh [adj] [(count adj) (conj adj [])])
          (edge [adj u v lab] (update adj u conj [v lab]))
          (gen [nd adj]
            (let [[in adj] (fresh adj)
                  [out adj] (fresh adj)
                  op (:op nd) ch (:ch nd) kids (:kids nd)]
              (cond
                (= op :concat)
                (if (empty? kids)
                  [in out (cond (= ch \a) (edge adj in out 1)
                                (= ch \b) (edge adj in out 2)
                                :else adj)]
                  (loop [ks kids prev-out nil fst true adj adj]
                    (if (empty? ks)
                      [in out (edge adj prev-out out 0)]
                      (let [[ci co adj] (gen (first ks) adj)
                            adj (if fst
                                  (edge adj in ci 0)
                                  (edge adj prev-out ci 0))]
                        (recur (rest ks) co false adj)))))

                (= op :closure)
                (if (empty? kids)
                  (let [[si adj] (fresh adj)
                        [so adj] (fresh adj)
                        adj (cond (= ch \a) (edge adj si so 1)
                                      (= ch \b) (edge adj si so 2)
                                      :else adj)]
                    [in out (-> adj
                                (edge in si 0)
                                (edge in out 0)
                                (edge so si 0)
                                (edge so out 0))])
                  (let [[si so adj]
                        (loop [ks kids sub-in nil prev-out nil fst true adj adj]
                          (if (empty? ks)
                            [sub-in prev-out adj]
                            (let [[ci co adj] (gen (first ks) adj)]
                              (if fst
                                (recur (rest ks) ci co false
                                       (edge adj in ci 0))
                                (recur (rest ks) sub-in co false
                                       (edge adj prev-out ci 0))))))]
                    [in out (-> adj
                                (edge in out 0)
                                (edge so out 0)
                                (edge so si 0))]))

                (= op :union)
                (let [[li lo adj] (gen (nth kids 0) adj)
                      [ri ro adj] (gen (nth kids 1) adj)]
                  [in out (-> adj
                              (edge in li 0)
                              (edge in ri 0)
                              (edge lo out 0)
                              (edge ro out 0))]))))]
    (let [[in out adj] (gen root [])]
      [adj in out])))

(defn- e-closure
  "Empty move closure of NFA state start as a sorted set."
  [adj start]
  (loop [q [start] qi 0 seen #{start}]
    (if (>= qi (count q))
      (into (sorted-set) seen)
      (let [u (nth q qi)
            nxt (remove seen
                        (map first (filter #(zero? (second %)) (nth adj u))))]
        (recur (into q nxt) (inc qi) (into seen nxt))))))

(defn- nfa-move
  "Set of states reached from set from over label tr."
  [adj from tr]
  (into (sorted-set)
        (comp (mapcat #(nth adj %))
              (filter #(= (second %) tr))
              (map first))
        from))

(defn- nfa-to-dfa
  "Subset construction over adj. Returns start, ends and rows."
  [adj nfa-in nfa-out]
  (let [s0 (e-closure adj nfa-in)]
    (loop [states [s0]
           idx {s0 0}
           dfa [nil]
           ends (if (contains? s0 nfa-out) #{0} #{})
           q [0]
           qi 0]
      (if (>= qi (count q))
        {:start 0 :ends ends :dfa dfa}
        (let [cur (nth q qi)
              curset (nth states cur)
              [states idx dfa ends q]
              (reduce (fn [[states idx dfa ends q] li]
                        (let [to (reduce into (sorted-set)
                                         (map #(e-closure adj %)
                                              (nfa-move adj curset (inc li))))
                              found (contains? idx to)
                              nxt (if found (idx to) (count states))
                              states (if found states (conj states to))
                              idx (if found idx (assoc idx to nxt))
                              dfa (if found dfa (conj dfa nil))
                              q (if found q (conj q nxt))
                              ends (if (contains? to nfa-out)
                                     (conj ends nxt)
                                     ends)
                              dfa (assoc dfa cur
                                         (assoc (or (nth dfa cur) [-1 -1]) li nxt))]
                          [states idx dfa ends q]))
                      [states idx dfa ends q] [0 1])]
          (recur states idx dfa ends q (inc qi)))))))

(defn- mmult
  "Product of flat n by n matrices a and b, modulo the shared prime."
  [a b n]
  (vec (for [x (range n) y (range n)]
         (let [xb (* x n)]
           (loop [i 0 s 0]
             (if (>= i n)
               s
               (let [av (nth a (+ xb i))]
                 (recur (inc i)
                        (if (zero? av)
                          s
                          (rem (+ s (* av (nth b (+ (* i n) y)))) MOD))))))))))

(defn- mexp
  "Flat n by n matrix m to the power p, modulo the shared prime."
  [m p n]
  (let [ident (vec (for [x (range n) y (range n)]
                     (if (= x y) 1 0)))]
    (loop [rp ident mp m p p]
      (if (<= p 0)
        rp
        (let [rp (if (odd? p) (mmult rp mp n) rp)
              p (bit-shift-right p 1)
              mp (if (> p 0) (mmult mp mp n) mp)]
          (recur rp mp p))))))

(defn- count-strings
  "Count of length l strings matching regex string r."
  [r l]
  (let [[adj nfa-in nfa-out] (build-nfa (parse-regex r))
        {:keys [dfa ends]} (nfa-to-dfa adj nfa-in nfa-out)
        n (count dfa)
        m (reduce (fn [m i]
                    (reduce (fn [m c] (update m (+ (* i n) c) inc))
                            m (nth dfa i)))
                  (vec (repeat (* n n) 0))
                  (range n))
        pw (mexp m l n)]
    (rem (reduce + 0 (map #(nth pw %) ends)) MOD)))

(defn -main
  "Parses data and prints one count per regex pair.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            t (parse-int (first toks) 0 2147483647)]
        (cond
          (nil? t)
          (report "error: expected a count followed by regex length pairs")
          :else
          (let [rest-toks (vec (rest toks))
                avail (count rest-toks)
                want (* 2 t)]
            (if (and (< avail want) (odd? avail))
              (report "error: expected a count followed by regex length pairs")
              (let [pairs (partition 2 (take (min want avail) rest-toks))
                    bad (some (fn [[r l]]
                                (nil? (parse-int l -9223372036854775808 9223372036854775807)))
                              pairs)]
                (if bad
                  (report "error: expected a decimal length per regex")
                  (let [out (map (fn [[r l]]
                                   (str (count-strings r (parse-int l -9223372036854775808 9223372036854775807))))
                                 pairs)]
                    (when (seq out)
                      (print (str (str/join "\n" out) "\n"))))))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
