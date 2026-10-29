;; Suffix automaton substring queries.
;; Input: string s, then a query count with that many pattern tokens.
;; Output: state, transition and distinct substring counts, then YES
;; or NO per query.
;; Linear expected time and space construction, linear time per
;; query in the pattern length, which is optimal here.
;; Pure clojure with no host interop.
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

(defn- sam-extend
  "Extends automaton [states last-idx] with char c, cloning a state
  when the old transition skips lengths. Returns [states cur]."
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
  "Builds the suffix automaton for string s. Returns [states last]."
  [s]
  (reduce sam-extend [[{:len 0 :link -1 :next {}}] 0] s))

(defn- transition-count
  "Total transition count over states."
  [states]
  (reduce + (map (comp count :next) states)))

(defn- distinct-substrings
  "Count of distinct substrings from state lengths and links."
  [states]
  (reduce + 0
          (map (fn [{:keys [len link]}]
                 (- len (if (= link -1) 0 (:len (states link)))))
               (rest states))))

(defn- sam-contains?
  "True when pattern pat occurs in the automaton language."
  [states pat]
  (loop [cur 0
         cs (seq pat)]
    (if (nil? cs)
      true
      (let [nxt (get (:next (states cur)) (first cs))]
        (if (nil? nxt)
          false
          (recur nxt (next cs)))))))

(defn -main
  "Parses data and prints the automaton summary plus query answers.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            [states _] (build-sam (first toks))
            head [(str "States: " (count states))
                  (str "Transitions: " (transition-count states))
                  (str "Distinct substrings: " (distinct-substrings states))]
            qr (next toks)]
        (if (nil? qr)
          (print (str (str/join "\n" head) "\n"))
          (let [q (parse-int (first qr) -2147483648 2147483647)]
            (if (nil? q)
              (report "error: expected a query count after the string")
              (let [tails (map #(if (sam-contains? states %) "YES" "NO")
                               (take (max 0 q) (rest qr)))]
                (print (str (str/join "\n" (concat head tails)) "\n")))))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
