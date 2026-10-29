;; Suffix automaton substring queries. Port of
;; cpp/src/string_related/suffix_automaton/suffix_automaton.cc.
;; Input: string s, then a query count with that many pattern tokens.
;; Output: state/transition/distinct-substring counts, then YES/NO per query.
(require '[clojure.string :as str])

(defn- sam-extend
  "Extends SAM [states last-idx] with character c, cloning a state when
  the existing transition skips lengths. Returns [states cur]."
  [[states last-idx] c]
  (let [cur (count states)
        states (conj states {:len (inc (:len (states last-idx)))
                             :link 0
                             :next {}})
        [states p] (loop [states states
                          p last-idx]
                     (if (or (= p -1) (contains? (:next (states p)) c))
                       [states p]
                       (recur (assoc-in states [p :next c] cur)
                              (:link (states p)))))]
    (if (= p -1)
      [states cur]
      (let [q (get-in states [p :next c])]
        (if (= (inc (:len (states p))) (:len (states q)))
          [(assoc-in states [cur :link] q) cur]
          (let [clone (count states)
                states (conj states (assoc (states q)
                                           :len (inc (:len (states p)))))
                states (loop [states states
                               p p]
                         (if (and (not (= p -1))
                                  (= (get-in states [p :next c]) q))
                           (recur (assoc-in states [p :next c] clone)
                                  (:link (states p)))
                           states))]
            [(-> states
                 (assoc-in [q :link] clone)
                 (assoc-in [cur :link] clone))
             cur]))))))

(defn- build-sam
  "Builds the suffix automaton for string s. Returns [states last-idx]."
  [s]
  (reduce sam-extend [[{:len 0 :link -1 :next {}}] 0] s))

(defn- transition-count [states]
  (reduce + (map (comp count :next) states)))

(defn- distinct-substrings [states]
  (reduce + 0
          (map (fn [{:keys [len link]}]
                 (- len (if (= link -1) 0 (:len (states link)))))
               (rest states))))

(defn- sam-contains? [states pat]
  (loop [cur 0
         cs (seq pat)]
    (if (nil? cs)
      true
      (let [nxt (get (:next (states cur)) (first cs))]
        (if (nil? nxt)
          false
          (recur nxt (next cs)))))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          [states _] (build-sam (first toks))
          sb (StringBuilder.)]
      (.append sb (str "States: " (count states) "\n"))
      (.append sb (str "Transitions: " (transition-count states) "\n"))
      (.append sb (str "Distinct substrings: " (distinct-substrings states) "\n"))
      (when-let [qr (next toks)]
        (let [q (Integer/parseInt (first qr))]
          (doseq [pat (take q (rest qr))]
            (.append sb (if (sam-contains? states pat) "YES\n" "NO\n")))))
      (print (.toString sb)))))
