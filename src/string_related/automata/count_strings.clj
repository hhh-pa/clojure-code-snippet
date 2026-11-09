;; Count strings of length l matching a regex over a and b symbols.
;; Input: a count t then t pairs of <regex> <l> tokens.
;; Output: one answer per line, modulo 1000000007.
(require '[clojure.string :as str])

(def MOD 1000000007)

(def digits
  "Digit chars mapped to their numeric values."
  (zipmap "0123456789" (range 10)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (if (nil? s)
    []
    (let [v (str/split s #"[ \t\n\r\f]+")
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
  (let [n (count r)]
    (loop [i 0 root [] stack []]
      (if (>= i n)
        {:op :concat :ch nil :kids root}
        (let [c (nth r i)]
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
  (letfn [(fresh [adj] [(count adj) (conj! adj [])])
          (edge [adj u v lab] (do (assoc! adj u (conj (nth adj u) [v lab])) adj))
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
    (let [[in out adj] (gen root (transient []))]
      [(persistent! adj) in out])))

(defn- e-closure
  "Empty move closure of NFA state start as a hash set."
  [adj start]
  (loop [q (transient [start]) qi 0 seen #{start}]
    (if (>= qi (count q))
      seen
      (let [u (nth q qi)
            es (nth adj u)
            ne (count es)
            [q seen] (loop [i 0 q q seen seen]
                       (if (= i ne)
                         [q seen]
                         (let [edge (nth es i)
                               v (nth edge 0)
                               lab (nth edge 1)]
                           (if (and (zero? lab) (not (contains? seen v)))
                             (recur (inc i) (conj! q v) (conj seen v))
                             (recur (inc i) q seen)))))]
        (recur q (inc qi) seen)))))

(defn- nfa-to-dfa
  "Subset construction over adj. Returns start, ends and rows."
  [adj nfa-in nfa-out]
  (let [n-nfa (count adj)
        ecls (mapv #(e-closure adj %) (range n-nfa))
        s0 (nth ecls nfa-in)]
    (loop [states (transient [s0])
           idx (transient {s0 0})
           dfa (transient [nil])
           ends (if (contains? s0 nfa-out) #{0} #{})
           q (transient [0])
           qi 0]
      (if (>= qi (count q))
        {:start 0 :ends ends :dfa (persistent! dfa)}
        (let [cur (nth q qi)
              curset (nth states cur)
              [states idx dfa ends q]
              (reduce (fn [[states idx dfa ends q] li]
                        (let [tr (inc li)
                              to (reduce (fn [acc u]
                                           (let [es (nth adj u)
                                                 ne (count es)]
                                             (loop [i 0 acc acc]
                                               (if (= i ne)
                                                 acc
                                                 (let [edge (nth es i)]
                                                   (if (= (nth edge 1) tr)
                                                     (recur (inc i) (into acc (nth ecls (nth edge 0))))
                                                     (recur (inc i) acc)))))))
                                         #{} curset)
                              found (contains? idx to)
                              nxt (if found (get idx to) (count states))
                              states (if found states (conj! states to))
                              idx (if found idx (assoc! idx to nxt))
                              dfa (if found dfa (conj! dfa nil))
                              q (if found q (conj! q nxt))
                              ends (if (contains? to nfa-out)
                                     (conj ends nxt)
                                     ends)
                              dfa (assoc! dfa cur
                                          (assoc (or (nth dfa cur) [-1 -1]) li nxt))]
                          [states idx dfa ends q]))
                      [states idx dfa ends q] [0 1])]
          (recur states idx dfa ends q (inc qi)))))))

(defn- minimize-dfa
  "Minimizes the 2-symbol DFA (start=0, ends set, dfa vector of [a-next b-next])
   via Moore partition refinement and prunes dead states that cannot reach ends."
  [dfa ends]
  (let [n (count dfa)
        init-cls (mapv #(if (contains? ends %) 1 0) (range n))
        cls (loop [cls init-cls prev-cnt -1]
              (let [[new-cls cnt]
                    (loop [i 0 mp {} out (transient []) next-id 0]
                      (if (= i n)
                        [(persistent! out) next-id]
                        (let [row (nth dfa i)
                              sig [(nth cls i)
                                   (nth cls (nth row 0))
                                   (nth cls (nth row 1))]
                              id (get mp sig -1)]
                          (if (not= id -1)
                            (recur (inc i) mp (conj! out id) next-id)
                            (recur (inc i) (assoc mp sig next-id) (conj! out next-id) (inc next-id))))))]
                (if (= cnt prev-cnt)
                  new-cls
                  (recur new-cls cnt))))
        num-cls (inc (reduce max 0 cls))
        rep (loop [i (dec n) r (vec (repeat num-cls 0))]
              (if (neg? i)
                r
                (recur (dec i) (assoc r (nth cls i) i))))
        min-dfa (mapv (fn [c]
                        (let [u (nth rep c)
                              row (nth dfa u)]
                          [(nth cls (nth row 0))
                           (nth cls (nth row 1))]))
                      (range num-cls))
        min-ends ( into #{} (for [c (range num-cls)
                                  :when (contains? ends (nth rep c))]
                              c))
        ;; Find which minimized states can reach min-ends
        alive (loop [alive min-ends]
                (let [alive2 (reduce (fn [acc c]
                                       (let [row (nth min-dfa c)]
                                         (if (or (contains? acc (nth row 0))
                                                 (contains? acc (nth row 1)))
                                           (conj acc c)
                                           acc)))
                                     alive
                                     (range num-cls))]
                  (if (= (count alive2) (count alive))
                    alive
                    (recur alive2))))]
    {:dfa min-dfa :ends min-ends :alive alive}))

(def MOD8 8000000112000000392)

(defn- vmult
  "Multiplies 1xN vector v by NxN matrix b modulo MOD."
  [v b n]
  (let [r (transient (vec (repeat n 0)))]
    (loop [k 0]
      (when (< k n)
        (let [vk (nth v k)]
          (when-not (zero? vk)
            (let [kb (* k n)]
              (loop [j 0]
                (when (< j n)
                  (let [bkj (nth b (+ kb j))]
                    (when-not (zero? bkj)
                      (let [s (+ (nth r j) (* vk bkj))]
                        (assoc! r j (if (>= s MOD8) (- s MOD8) s)))))
                  (recur (inc j)))))))
        (recur (inc k))))
    (loop [j 0]
      (when (< j n)
        (assoc! r j (rem (nth r j) MOD))
        (recur (inc j))))
    (persistent! r)))

(defn- mmult
  [a b n]
  (let [r (transient (vec (repeat (* n n) 0)))]
    (loop [i 0]
      (when (< i n)
        (let [xb (* i n)]
          (loop [k 0]
            (when (< k n)
              (let [aik (nth a (+ xb k))]
                (when-not (zero? aik)
                  (let [kb (* k n)]
                    (loop [j 0]
                      (when (< j n)
                        (let [bkj (nth b (+ kb j))]
                          (when-not (zero? bkj)
                            (let [o (+ xb j)
                                  s (+ (nth r o) (* aik bkj))]
                              (assoc! r o (if (>= s MOD8) (- s MOD8) s)))))
                        (recur (inc j)))))))
              (recur (inc k))))
          (loop [j 0]
            (when (< j n)
              (let [o (+ xb j)]
                (assoc! r o (rem (nth r o) MOD)))
              (recur (inc j)))))
        (recur (inc i))))
    (persistent! r)))

(defn- mexp-row0
  "Computes row 0 of m^p modulo MOD using O(n^2) vector-matrix steps for odd bits."
  [m p n]
  (let [row0 (assoc (vec (repeat n 0)) 0 1)]
    (loop [rv row0 mp m p p]
      (if (<= p 0)
        rv
        (let [rv (if (odd? p) (vmult rv mp n) rv)
              p (bit-shift-right p 1)
              mp (if (> p 0) (mmult mp mp n) mp)]
          (recur rv mp p))))))

(defn- count-strings
  [r l]
  (let [[adj nfa-in nfa-out] (build-nfa (parse-regex r))
        {:keys [dfa ends]} (nfa-to-dfa adj nfa-in nfa-out)
        {:keys [dfa ends alive]} (minimize-dfa dfa ends)]
    (if (not (contains? alive 0))
      0
      (let [alive-vec (vec (sort alive))
            n (count alive-vec)
            remap (into {} (map-indexed (fn [idx old-c] [old-c idx]) alive-vec))
            m (persistent!
                (reduce (fn [m idx]
                          (let [old-c (nth alive-vec idx)
                                row (nth dfa old-c)]
                            (reduce (fn [m nxt-c]
                                      (if-let [nxt-idx (get remap nxt-c)]
                                        (let [cell (+ (* idx n) nxt-idx)]
                                          (assoc! m cell (inc (nth m cell))))
                                        m))
                                    m row)))
                        (transient (vec (repeat (* n n) 0)))
                        (range n)))
            row0 (mexp-row0 m l n)]
        (rem (reduce (fn [acc old-e]
                       (if-let [e-idx (get remap old-e)]
                         (+ acc (nth row0 e-idx))
                         acc))
                     0 ends)
             MOD)))))

(defn -main
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            t (parse-int (first toks) 0 2147483647)]
        (cond
          (nil? t)
          (report "error: expected a count followed by regex length pairs")
          :else
          (let [rest-toks (subvec toks 1)
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
