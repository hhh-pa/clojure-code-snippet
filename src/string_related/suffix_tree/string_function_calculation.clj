;; String function calculation with a suffix tree: max over all
;; distinct substrings of length times occurrences.
;; Input: a single lowercase string token. Output: the maximum.
;; Linear time and space with the online tree method over the string
;; plus a terminal mark; the answer is the max over tree nodes of
;; string depth times subtree leaf count.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

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

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- lower-string?
  "True when every char of s is a lowercase letter."
  [s]
  (and (seq s) (every? #(contains? alpha-index %) s)))

(defn- node-end
  "End index of node, resolving open leaves to leaf-end."
  [node leaf-end]
  (if (:leaf node) leaf-end (:end node)))

(defn- edge-len
  "Length of the edge into node u with leaves open to leaf-end."
  [nodes u leaf-end]
  (if (zero? u)
    0
    (let [nd (nth nodes u)]
      (inc (- (node-end nd leaf-end) (:start nd))))))

(defn- add-node
  "Appends a node and returns [nodes new-index]."
  [nodes start end leaf?]
  [(conj nodes {:link 0 :start start :end end :leaf leaf? :next {}})
   (count nodes)])

(defn- follow-link
  "Active point step after an extension, following suffix links."
  [nodes active edge alen pos rem]
  (cond
    (and (zero? active) (pos? alen)) [active (inc (- pos rem)) (dec alen)]
    (zero? active) [active edge alen]
    :else [(:link (nth nodes active)) edge alen]))

(defn- build-tree
  "Suffix tree nodes for char vector t with the online method."
  [t]
  (let [n (count t)]
    (loop [pos 0
           nodes [{:link 0 :start 0 :end -1 :leaf false :next {}}]
           active 0 edge 0 alen 0 rem 0]
      (if (>= pos n)
        nodes
        (let [[nodes active edge alen rem]
              (loop [nodes nodes active active edge edge
                     alen alen rem (inc rem) last-new -1]
                (if (zero? rem)
                  [nodes active edge alen rem]
                  (let [edge (if (zero? alen) pos edge)
                        cur-char (nth t edge)
                        nxt (get (:next (nth nodes active)) cur-char)]
                    (if (nil? nxt)
                      (let [[nodes leaf] (add-node nodes pos -1 true)
                            nodes (assoc-in nodes [active :next cur-char] leaf)
                            nodes (if (not= last-new -1)
                                    (assoc-in nodes [last-new :link] active)
                                    nodes)
                            rem (dec rem)
                            [active edge alen]
                            (follow-link nodes active edge alen pos rem)]
                        (recur nodes active edge alen rem -1))
                      (let [elen (edge-len nodes nxt pos)]
                        (if (>= alen elen)
                          (recur nodes nxt (+ edge elen) (- alen elen)
                                 rem last-new)
                          (let [epos (+ (:start (nth nodes nxt)) alen)]
                            (if (= (nth t epos) (nth t pos))
                              [(if (and (not= last-new -1) (not (zero? active)))
                                 (assoc-in nodes [last-new :link] active)
                                 nodes)
                               active edge (inc alen) rem]
                              (let [split-end (+ (:start (nth nodes nxt)) alen -1)
                                    [nodes split]
                                    (add-node nodes (:start (nth nodes nxt))
                                              split-end false)
                                    [nodes leaf] (add-node nodes pos -1 true)
                                    nodes (-> nodes
                                              (assoc-in [active :next cur-char] split)
                                              (assoc-in [split :next (nth t pos)] leaf)
                                              (update-in [nxt :start] + alen)
                                              (assoc-in [split :next (nth t epos)] nxt))
                                    nodes (if (not= last-new -1)
                                            (assoc-in nodes [last-new :link] split)
                                            nodes)
                                    rem (dec rem)
                                    [active edge alen]
                                    (follow-link nodes active edge alen pos rem)]
                                (recur nodes active edge alen rem split))))))))))]
          (recur (inc pos) nodes active edge alen rem))))))

(defn- tree-value
  "Max over tree nodes of string depth times leaf count, where n is
  the input length without the terminal mark."
  [nodes n]
  (let [nst (count nodes)
        order (loop [stack [0] ord []]
                (if (empty? stack)
                  ord
                  (let [u (peek stack)]
                    (recur (into (pop stack) (vals (:next (nth nodes u))))
                           (conj ord u)))))
        depth (reduce (fn [dep v]
                        (let [dv (nth dep v)]
                          (reduce (fn [d w]
                                    (let [nd (nth nodes w)]
                                      (assoc d w (+ dv (if (:leaf nd)
                                                         (- n (:start nd))
                                                         (inc (- (:end nd)
                                                                 (:start nd))))))))
                                  dep
                                  (vals (:next (nth nodes v))))))
                      (vec (repeat nst 0))
                      order)
        [_ ans] (reduce (fn [[cnt ans] v]
                          (let [nd (nth nodes v)
                                c (if (:leaf nd)
                                    1
                                    (reduce + 0 (map cnt (vals (:next nd)))))]
                            [(assoc cnt v c)
                             (max ans (* (nth depth v) c))]))
                        [(vec (repeat nst 0)) n]
                        (reverse order))]
    ans))

(defn- string-value
  "Max over distinct substrings of length times occurrences."
  [s]
  (let [n (count s)]
    (if (zero? n)
      0
      (tree-value (build-tree (conj (vec s) \$)) n))))

(defn -main
  "Parses data and prints the string function value.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            s (first toks)]
        (if (or (nil? s) (not (lower-string? s)))
          (report "error: expected a single lowercase string token")
          (println (string-value s))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
