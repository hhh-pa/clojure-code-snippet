;; Count strings of length l matching a regex over {a,b}, mod 1000000007.
;; Port of cpp/src/string_related/automata/count_strings.cc:
;; regex -> AST -> NFA (Thompson) -> DFA (subset construction) ->
;; adjacency-matrix exponentiation. All products use 64-bit arithmetic with a
;; mod reduction after every add, exactly like the reference.
;; Input: t then t pairs of <regex> <l> tokens. Output: one answer per line.
(require '[clojure.string :as str])

(def ^:const MOD 1000000007)

;; ---------------- AST ----------------
;; Nodes: {:op :concat|:closure|:union, :ch nil|char, :kids [...]}.
;; The parser mirrors parse_string in the .cc, including its treatment of '*'
;; (it flips the op of the last child node in place, even a Union node).
(defn- attach [kids node]
  (let [n (count kids)]
    (if (and (pos? n)
             (let [b (peek kids)]
               (and (= (:op b) :union) (= (count (:kids b)) 1))))
      (assoc kids (dec n) (update (peek kids) :kids conj node))
      (conj kids node))))

(defn- parse-regex [^String r]
  (let [n (.length r)]
    (loop [i (int 0) root [] stack []]
      (if (>= i n)
        {:op :concat :ch nil :kids root}
        (let [c (.charAt r i)]
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

;; ---------------- NFA ----------------
;; adj: vector of vectors of [to label]; labels: 0 = epsilon, 1 = 'a', 2 = 'b'.
(defn- build-nfa [root]
  (let [adj (atom [])
        add! (fn add! [] (let [id (count @adj)] (swap! adj conj []) id))
        edge! (fn edge! [u v lab] (swap! adj update u conj [v lab]))
        gen (fn gen [nd]
              (let [in (add!) out (add!)
                    op (:op nd) ch (:ch nd) kids (:kids nd)]
                (cond
                  (= op :concat)
                  (if (empty? kids)
                    (cond (= ch \a) (edge! in out 1)
                          (= ch \b) (edge! in out 2))
                    (loop [ks kids prev-out nil fst true]
                      (if (empty? ks)
                        (edge! prev-out out 0)
                        (let [[ci co] (gen (first ks))]
                          (if fst (edge! in ci 0) (edge! prev-out ci 0))
                          (recur (rest ks) co false)))))

                  (= op :closure)
                  (if (empty? kids)
                    (let [si (add!) so (add!)]
                      (cond (= ch \a) (edge! si so 1)
                            (= ch \b) (edge! si so 2))
                      (edge! in si 0)
                      (edge! in out 0)
                      (edge! so si 0)
                      (edge! so out 0))
                    (let [[si so] (loop [ks kids sub-in nil prev-out nil fst true]
                                    (if (empty? ks)
                                      [sub-in prev-out]
                                      (let [[ci co] (gen (first ks))]
                                        (if fst
                                          (do (edge! in ci 0)
                                              (recur (rest ks) ci co false))
                                          (do (edge! prev-out ci 0)
                                              (recur (rest ks) sub-in co false))))))]
                      (edge! in out 0)
                      (edge! so out 0)
                      (edge! so si 0)))

                  (= op :union)
                  (let [[li lo] (gen (nth kids 0))
                        [ri ro] (gen (nth kids 1))]
                    (edge! in li 0)
                    (edge! in ri 0)
                    (edge! lo out 0)
                    (edge! ro out 0)))
                [in out]))]
    (let [[in out] (gen root)]
      [@adj in out])))

(defn- e-closure [adj start]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY start) seen #{start}]
    (if (empty? q)
      (into (sorted-set) seen)
      (let [u (peek q)
            nxt (remove seen (map first (filter #(zero? (second %)) (nth adj u))))]
        (recur (into (pop q) nxt) (into seen nxt))))))

(defn- nfa-move [adj from tr]
  (into (sorted-set)
        (comp (mapcat #(nth adj %))
              (filter #(= (second %) tr))
              (map first))
        from))

;; ---------------- DFA (subset construction) ----------------
(defn- nfa->dfa [adj nfa-in nfa-out]
  (let [s0 (e-closure adj nfa-in)]
    (loop [states [s0]
           idx {s0 0}
           dfa [nil]
           ends (if (contains? s0 nfa-out) #{0} #{})
           q (conj clojure.lang.PersistentQueue/EMPTY 0)]
      (if (empty? q)
        {:start 0 :ends ends :dfa dfa}
        (let [cur (peek q)
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
                              ends (if (contains? to nfa-out) (conj ends nxt) ends)
                              dfa (assoc dfa cur (assoc (or (nth dfa cur) [-1 -1]) li nxt))]
                          [states idx dfa ends q]))
                      [states idx dfa ends (pop q)] [0 1])]
          (recur states idx dfa ends q))))))

;; ---------------- matrix exponentiation (flat long arrays, mod MOD) ----------------
(defn- mmult [^longs a ^longs b ^long n ^long mod]
  (let [r (long-array (* n n))]
    (loop [x (long 0)]
      (when (< x n)
        (let [xb (* x n)]
          (loop [y (long 0)]
            (when (< y n)
              (aset r (+ xb y)
                    (loop [i (long 0) s (long 0)]
                      (if (>= i n)
                        s
                        (let [av (aget a (+ xb i))]
                          (recur (unchecked-inc i)
                                 (if (zero? av)
                                   s
                                   (rem (unchecked-add s (unchecked-multiply av (aget b (+ (* i n) y))))
                                        mod)))))))
              (recur (unchecked-inc y)))))
        (recur (unchecked-inc x))))
    r))

(defn- mexp [^longs m ^long p ^long n ^long mod]
  (let [r (long-array (* n n))]
    (loop [i (long 0)]
      (when (< i n)
        (aset r (+ (* i n) i) 1)
        (recur (unchecked-inc i))))
    (loop [rp r mp m p p]
      (if (<= p 0)
        rp
        (let [rp (if (odd? p) (mmult rp mp n mod) rp)
              p (bit-shift-right p 1)
              mp (if (> p 0) (mmult mp mp n mod) mp)]
          (recur rp mp p))))))

(defn- count-strings [^String r ^long l]
  (let [[adj nfa-in nfa-out] (build-nfa (parse-regex r))
        {:keys [dfa ends]} (nfa->dfa adj nfa-in nfa-out)
        n (long (count dfa))
        m (long-array (* n n))]
    (dotimes [i n]
      (doseq [c (nth dfa (int i))]
        (let [j (+ (* i n) (long c))]
          (aset m j (inc (aget m j))))))
    (let [pw (mexp m l n MOD)]
      (rem (reduce + 0 (map #(aget pw (long %)) ends)) MOD))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          t (Integer/parseInt (first toks))
          sb (StringBuilder.)]
      (loop [ps (take (* 2 t) (rest toks))]
        (when (seq ps)
          (let [^String r (first ps)
                l (Long/parseLong (second ps))]
            (.append sb (Long/toString (long (count-strings r l))))
            (.append sb "\n")
            (recur (drop 2 ps)))))
      (print (.toString sb)))))
