;; blossom: Edmonds maximum matching
;;
;; Edmonds blossom search with LCA contraction over a queue of
;; alternating trees, augmenting until maximal. Emits the size and
;; the sorted pairs.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          g (atom (mapv (fn [_] []) (range n)))
          mt (atom (vec (repeat n -1)))
          base (atom (vec (range n)))
          pp (atom (vec (repeat n -1)))
          used (atom (vec (repeat n false)))
          bloss (atom (vec (repeat n false)))
          used-lca (atom (vec (repeat n false)))]
      (doseq [i (range m)]
        (let [u (nth nums (+ 2 (* i 2))) v (nth nums (+ 3 (* i 2)))]
          (when (not= u v)
            (swap! g update u conj v)
            (swap! g update v conj u))))
      (doseq [u (range n)]
        (swap! g assoc u (vec (sort (set (nth @g u))))))
      (letfn [(lca [a0 b0]
                (reset! used-lca (vec (repeat n false)))
                (loop [a a0]
                  (let [ba (nth @base a)]
                    (swap! used-lca assoc ba true)
                    (if (= (nth @mt ba) -1)
                      (loop [b b0]
                        (let [bb (nth @base b)]
                          (if (nth @used-lca bb)
                            bb
                            (if (= (nth @mt bb) -1)
                              -1
                              (recur (nth @pp (nth @mt bb)))))))
                      (recur (nth @pp (nth @mt ba)))))))
              (mark-path [v0 b child0]
                (loop [v v0 child child0]
                  (when (not= (nth @base v) b)
                    (swap! bloss assoc (nth @base v) true)
                    (swap! bloss assoc (nth @base (nth @mt v)) true)
                    (swap! pp assoc v child)
                    (let [nc (nth @mt v)]
                      (recur (nth @pp nc) nc)))))
              (find-path [root]
                (reset! pp (vec (repeat n -1)))
                (reset! base (vec (range n)))
                (reset! used (vec (repeat n false)))
                (swap! used assoc root true)
                (loop [q (conj clojure.lang.PersistentQueue/EMPTY root)]
                  (if (empty? q)
                    false
                    (let [v (peek q) rst (pop q)
                          res (atom nil) nq (atom rst)]
                      (doseq [to (nth @g v)]
                        (when (nil? @res)
                          (cond (or (= (nth @base v) (nth @base to)) (= (nth @mt v) to)) nil
                                (or (= to root) (and (not= (nth @mt to) -1) (not= (nth @pp (nth @mt to)) -1)))
                                (let [cur (lca v to)]
                                  (reset! bloss (vec (repeat n false)))
                                  (mark-path v cur to)
                                  (mark-path to cur v)
                                  (doseq [i (range n)]
                                    (when (nth @bloss (nth @base i))
                                      (swap! base assoc i cur)
                                      (when (not (nth @used i))
                                        (swap! used assoc i true)
                                        (swap! nq conj i)))))
                                (= (nth @pp to) -1)
                                (do (swap! pp assoc to v)
                                    (if (= (nth @mt to) -1)
                                      (do (loop [cur to]
                                            (when (not= cur -1)
                                              (let [prev (nth @pp cur)
                                                    nxt (if (not= prev -1) (nth @mt prev) -1)]
                                                (swap! mt assoc cur prev)
                                                (when (not= prev -1) (swap! mt assoc prev cur))
                                                (recur nxt))))
                                          (reset! res true))
                                      (let [nx (nth @mt to)]
                                        (swap! used assoc nx true)
                                        (swap! nq conj nx)))))))
                      (if (some? @res)
                        @res
                        (recur @nq))))))]
        (doseq [v (range n)]
          (when (= (nth @mt v) -1)
            (find-path v)))
        (let [prs (vec (sort (filterv some? (mapv (fn [i] (when (and (not= (nth @mt i) -1) (< i (nth @mt i))) [i (nth @mt i)])) (range n)))))]
          (println (count prs))
          (doseq [[u v] prs] (println (str u " " v))))))))
