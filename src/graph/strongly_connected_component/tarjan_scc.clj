;; Tarjan strongly connected components in pure clojure. One
;; depth first search tracks discovery numbers and low links in
;; O(V + E) time and O(V + E) space, the best known bound for this
;; problem. The old recursive walk is replaced by an explicit frame
;; stack holding parent links and child positions, so deep graphs
;; cannot overflow the call stack while the bound stays the same.
;; Every component prints sorted, and components print ordered by
;; first vertex, exactly like the old run.
;; Input shape: n m, then m directed edges u v with 0 based verts.
;; Output is the component count plus one line per component.
;; Blank stdin prints nothing. Badly shaped input prints one
;; message to stderr and returns with exit 0, since a nonzero exit
;; needs host interop, which this script avoids by design.

(require 'clojure.string)

(def digit-values
  "Maps each digit char to its numeric value for the tokenizer below."
  (zipmap "0123456789" (range)))

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


(defn parse-ints
  "Reads all signed integers from string s into a vector. Chars are
   tested with core compare fns after int coercion, digits through
   digit-values, and anything else fails the whole scan with nil."
  [s]
  (let [done
        (reduce
         (fn [state ch]
           (let [nums (:nums state)
                 cur (:cur state)
                 neg (:neg state)
                 bad (:bad state)
                 code (int ch)]
             (cond
               bad state
               (<= (int \0) code (int \9))
               (assoc state :cur (+' (*' (or cur 0) 10) (digit-values ch)))
               (= ch \-)
               (if (or (some? cur) neg)
                 (assoc state :bad true)
                 (assoc state :neg true))
               (<= code (int \space))
               (cond
                 (some? cur)
                 (let [v (fit-long (if neg (-' cur) cur))]
                   (if (some? v)
                     (-> state
                     (assoc :nums (conj! nums v))
                     (assoc :cur nil)
                     (assoc :neg false))
                     (assoc state :bad true)))
                 neg (assoc state :bad true)
                 :else state)
               :else (assoc state :bad true))))
         {:nums (transient []) :cur nil :neg false :bad false}
         (seq s))]
    (when-not (:bad done)
      (let [nums (:nums done)
            cur (:cur done)
            neg (:neg done)]
        (cond
          (some? cur) (let [v (fit-long (if neg (-' cur) cur))]
                        (when (some? v) (persistent! (conj! nums v))))
          neg nil
          :else (persistent! nums))))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn tarjan-sccs
  "Finds all components over adj with an explicit frame stack. Each
   frame holds [vertex parent next child index]. Entry numbering,
   low link updates, and root popping match true recursion. Returns
   the component list with sorted members."
  [adj n]
  (let [dfn (transient (vec (repeat n 0)))
        low (transient (vec (repeat n 0)))
        on-stack (transient (vec (repeat n false)))]
    (loop [start 0 timer 0 stack [] sccs (transient [])
           dfn dfn low low on-stack on-stack]
      (if (>= start n)
        (persistent! sccs)
        (if (not= (nth dfn start) 0)
          (recur (inc start) timer stack sccs dfn low on-stack)
          (let [timer (inc timer)
                dfn (assoc! dfn start timer)
                low (assoc! low start timer)
                on-stack (assoc! on-stack start true)
                [timer stack sccs dfn low on-stack]
                (loop [frames [[start -1 0]]
                       timer timer
                       stack (conj stack start)
                       sccs sccs dfn dfn low low on-stack on-stack]
                  (if (empty? frames)
                    [timer stack sccs dfn low on-stack]
                    (let [[u parent idx] (peek frames)
                          nbrs (nth adj u)]
                      (if (< idx (count nbrs))
                        (let [v (nth nbrs idx)
                              frames (conj (pop frames) [u parent (inc idx)])]
                          (cond
                            (zero? (nth dfn v))
                            (let [timer (inc timer)]
                              (recur (conj frames [v u 0])
                                     timer
                                     (conj stack v)
                                     sccs
                                     (assoc! dfn v timer)
                                     (assoc! low v timer)
                                     (assoc! on-stack v true)))
                            (nth on-stack v)
                            (recur frames timer stack sccs dfn
                                   (assoc! low u (min (nth low u)
                                                      (nth dfn v)))
                                   on-stack)
                            :else
                            (recur frames timer stack sccs
                                   dfn low on-stack)))
                        (let [[stack scc on-stack]
                              (if (= (nth low u) (nth dfn u))
                                (loop [stack stack scc [] on-stack on-stack]
                                  (let [w (peek stack)
                                        stack (pop stack)
                                        on-stack (assoc! on-stack w false)
                                        scc (conj scc w)]
                                    (if (= w u)
                                      [stack scc on-stack]
                                      (recur stack scc on-stack))))
                                [stack nil on-stack])
                              sccs (if (nil? scc)
                                     sccs
                                     (conj! sccs (vec (sort scc))))
                              frames (pop frames)
                              low (if (= parent -1)
                                    low
                                    (assoc! low parent
                                            (min (nth low parent)
                                                 (nth low u))))]
                          (recur frames timer stack sccs
                                 dfn low on-stack))))))]
            (recur (inc start) timer stack sccs dfn low on-stack)))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 0) (< m 0) (< c (+' 2 (*' m 2))))
      (report-error "bad input: want n m plus m edge pairs")
      (if (zero? n)
        (do (println 0) nil)
        (let [adj (loop [i 0 adj (transient (vec (repeat n [])))]
                    (if (>= i m)
                      (persistent! adj)
                      (let [u (nth nums (+ 2 (* i 2)))
                            v (nth nums (+ 3 (* i 2)))]
                        (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                          nil
                          (recur (inc i)
                                 (assoc! adj u
                                         (conj (nth adj u) v)))))))]
          (if (nil? adj)
            (report-error "bad input: edge vertex out of range")
            (let [sccs (sort-by first (tarjan-sccs adj n))
                  lines (transient [(str (count sccs))])]
              (loop [ss sccs lines lines]
                (if (empty? ss)
                  (do (println (clojure.string/join "\n" (persistent! lines)))
                      nil)
                  (recur (rest ss)
                         (conj! lines (clojure.string/join
                                       " " (map str (first ss))))))))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
