;; LALR(1) table construction + parsing (Louden 5.4.3).
(require '[clojure.string :as str])

(defn- lalr-first-add
  [fst lhs rhs]
  (if (empty? rhs)
    (if (contains? (get fst lhs) nil)
      [fst false]
      [(assoc fst lhs (conj (get fst lhs) nil)) true])
    (loop [ss rhs f fst changed false]
      (if (empty? ss)
        (if (contains? (get f lhs) nil)
          [f changed]
          [(assoc f lhs (conj (get f lhs) nil)) true])
        (let [s (first ss)]
          (if (not (contains? f s))
            (if (contains? (get f lhs) s)
              [f changed]
              [(assoc f lhs (conj (get f lhs) s)) true])
            (let [before (count (get f lhs))
                  f2 (assoc f lhs (into (get f lhs) (disj (get f s) nil)))
                  ch (or changed (not= (count (get f2 lhs)) before))]
              (if (contains? (get f s) nil)
                (recur (rest ss) f2 ch)
                [f2 ch]))))))))

(defn- lalr-first-sets
  [prods]
  (loop [fst (into {} (map (fn [[lhs _]] [lhs #{}]) prods))]
    (let [[f2 changed] (reduce (fn [[f c] [lhs rhs]]
                                 (let [[nf ch] (lalr-first-add f lhs rhs)]
                                   [nf (or c ch)]))
                               [fst false] prods)]
      (if changed (recur f2) f2))))

(defn- lalr-first-seq
  [fst seq]
  (loop [ss seq out #{}]
    (if (empty? ss)
      out
      (let [s (first ss)]
        (if (not (contains? fst s))
          (conj out s)
          (let [out (into out (disj (get fst s) nil))]
            (if (contains? (get fst s) nil)
              (recur (rest ss) out)
              out)))))))

(defn- lalr-closure
  [prods fst items]
  (loop [out (into (sorted-set) items)
         work (vec items)]
    (if (empty? work)
      out
      (let [[p d la] (peek work)
            work (pop work)
            rhs (nth (nth prods p) 1)]
        (if (or (>= d (count rhs)) (not (contains? fst (nth rhs d))))
          (recur out work)
          (let [b (nth rhs d)
                fset (lalr-first-seq fst (conj (vec (drop (inc d) rhs)) la))
                adds (for [q (range (count prods))
                           :when (= (nth (nth prods q) 0) b)
                           x fset
                           :when (not (contains? out [q 0 x]))]
                       [q 0 x])]
            (recur (into out adds) (into work adds))))))))

(defn- lalr-goto
  [prods fst items sym]
  (let [moved (into (sorted-set)
                    (comp (filter (fn [[p d _]]
                                    (let [rhs (nth (nth prods p) 1)]
                                      (and (< d (count rhs)) (= (nth rhs d) sym)))))
                          (map (fn [[p d la]] [p (inc d) la])))
                    items)]
    (when (seq moved)
      (lalr-closure prods fst moved))))

(defn- lalr-build
  [aug fst terms nonterms]
  (loop [states [(lalr-closure aug fst (sorted-set [0 0 "$"]))]
         index {(lalr-closure aug fst (sorted-set [0 0 "$"])) 0}
         trans {}
         queue [0]]
    (if (empty? queue)
      [states trans]
      (let [si (first queue)
            queue (vec (rest queue))
            [states index trans queue]
            (reduce (fn [[states index trans queue] x]
                      (let [g (lalr-goto aug fst (nth states si) x)]
                        (if (nil? g)
                          [states index trans queue]
                          (if (contains? index g)
                            [states index (assoc trans [si x] (get index g)) queue]
                            (let [t (count states)]
                              [(conj states g) (assoc index g t)
                               (assoc trans [si x] t) (conj queue t)])))))
                    [states index trans queue]
                    (concat terms nonterms))]
        (recur states index trans queue)))))

(defn- lalr-core
  [st]
  (set (map (fn [[p d _]] [p d]) st)))

(defn- lalr-tables
  [aug states trans terms nonterms]
  (let [term-set (set terms)
        nonterm-set (set nonterms)
        cores (vec (distinct (map lalr-core states)))
        cls-of (mapv #(.indexOf cores (lalr-core %)) states)
        members (reduce (fn [m [si ci]] (update m ci (fnil conj []) si))
                        (vec (repeat (count cores) []))
                        (map-indexed vector cls-of))]
    (loop [ci 0 action (sorted-map) goto (sorted-map) conflict false]
      (if (>= ci (count members))
        [action goto (count members) conflict (nth cls-of 0)]
        (let [mem (nth members ci)
              look (reduce (fn [m si]
                             (reduce (fn [m2 [p d la]]
                                       (update m2 [p d] (fnil conj #{}) la))
                                     m (nth states si)))
                           {} mem)
              [action conflict]
              (reduce (fn [[ac cf] [[p d] las]]
                        (let [rhs (nth (nth aug p) 1)]
                          (cond
                            (and (< d (count rhs)) (contains? term-set (nth rhs d)))
                            (let [tgt (some (fn [si]
                                              (when (contains? trans [si (nth rhs d)])
                                                (nth cls-of (get trans [si (nth rhs d)]))))
                                            mem)
                                  ent [:s tgt]
                                  k [ci (nth rhs d)]]
                              (if (and (contains? ac k) (not= (get ac k) ent))
                                [(assoc ac k ent) true]
                                [(assoc ac k ent) cf]))
                            (and (= d (count rhs)) (pos? p))
                            (reduce (fn [[ac cf] la]
                                      (let [ent [:r p]
                                            k [ci la]]
                                        (if (and (contains? ac k) (not= (get ac k) ent))
                                          [(assoc ac k ent) true]
                                          [(assoc ac k ent) cf])))
                                    [ac cf] las)
                            (and (= d (count rhs)) (zero? p))
                            (let [ent [:acc 0]
                                  k [ci "$"]]
                              (if (and (contains? ac k) (not= (get ac k) ent))
                                [(assoc ac k ent) true]
                                [(assoc ac k ent) cf]))
                            :else [ac cf])))
                      [action conflict] look)
              goto (reduce (fn [g [[p d] _]]
                             (let [rhs (nth (nth aug p) 1)]
                               (if (and (< d (count rhs))
                                        (contains? nonterm-set (nth rhs d)))
                                 (if-let [t (some (fn [si]
                                                    (when (contains? trans [si (nth rhs d)])
                                                      (nth cls-of (get trans [si (nth rhs d)]))))
                                                  mem)]
                                   (assoc g [ci (nth rhs d)] t)
                                   g)
                                 g)))
                           goto look)]
          (recur (inc ci) action goto conflict))))))

(defn- lalr-parse
  [aug action goto start toks]
  (loop [stack [start] pos 0 reds []]
    (let [ent (get action [(peek stack) (nth toks pos)])]
      (cond
        (nil? ent) nil
        (= (first ent) :s) (recur (conj stack (second ent)) (inc pos) reds)
        (= (first ent) :r) (let [p (second ent)
                                 rhs (nth (nth aug p) 1)
                                 st (vec (drop-last (count rhs) stack))
                                 g (get goto [(peek st) (nth (nth aug p) 0)])]
                             (if (nil? g)
                               nil
                               (recur (conj st g) pos (conj reds p))))
        :else reds))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [lines (str/split-lines s)
          m (Long/parseLong (first (str/split (nth lines 0) #"\s+")))
          prods (mapv (fn [i]
                        (let [toks (str/split (nth lines (inc i)) #"\s+")]
                          [(first toks) (vec (drop 2 toks))]))
                      (range m))
          q (Long/parseLong (first (str/split (nth lines (inc m)) #"\s+")))
          queries (mapv (fn [i]
                          (let [ln (nth lines (+ 2 m i) "")]
                            (if (str/blank? ln) [] (vec (str/split ln #"\s+")))))
                        (range q))
          nonterms (vec (distinct (map first prods)))
          terms (vec (distinct (filter #(not (contains? (set nonterms) %))
                                       (mapcat second prods))))
          fst (lalr-first-sets prods)
          aug (vec (cons ["$S" [(first (first prods))]] prods))
          fst (assoc fst "$S" #{(first (first prods))})
          [states trans] (lalr-build aug fst terms nonterms)
          [action goto ncls conflict start] (lalr-tables aug states trans terms nonterms)]
      (if conflict
        (print "CONFLICT")
        (let [out (atom [(str "STATES " ncls)])]
          (doseq [qtoks queries]
            (let [reds (lalr-parse aug action goto start (conj (vec qtoks) "$"))]
              (swap! out conj (if (nil? reds)
                                "REJECT"
                                (if (empty? reds)
                                  "ACCEPT"
                                  (str "ACCEPT " (str/join " " reds)))))))
          (print (str/join "\n" @out)))))))
