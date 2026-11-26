;; ABD multi-writer atomic register emulation (Lynch 17.1.3 + MWMR).
(require '[clojure.string :as str])

(defn- abd-write
  [correct val tag p v]
  (let [s (reduce max 0 (map #(nth % 0) (map tag correct)))
        nt [ (inc s) p]
        upd (reduce (fn [[vl tg] r]
                      (if (pos? (compare nt (nth tg r)))
                        [(assoc vl r v) (assoc tg r nt)]
                        [vl tg]))
                    [val tag] correct)]
    [(nth upd 0) (nth upd 1) (str "ACK " (inc s) " " p)]))

(defn- abd-read
  [correct val tag]
  (let [[best bv] (reduce (fn [[bt bv] r]
                            (if (pos? (compare (nth tag r) bt))
                              [(nth tag r) (nth val r)]
                              [bt bv]))
                          [[0 0] 0] correct)
        upd (reduce (fn [[vl tg] r]
                      (if (pos? (compare best (nth tg r)))
                        [(assoc vl r bv) (assoc tg r best)]
                        [vl tg]))
                    [val tag] correct)]
    [(nth upd 0) (nth upd 1) (str "VAL " bv " " (nth best 0) " " (nth best 1))]))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          n (Long/parseLong (next-tok))
          m (Long/parseLong (next-tok))
          k (Long/parseLong (next-tok))
          crashed (set (repeatedly k #(Long/parseLong (next-tok))))
          correct (vec (remove crashed (range n)))
          quorum (inc (quot n 2))]
      (loop [t 0 val (vec (repeat n 0)) tag (vec (repeat n [0 0])) out []]
        (if (>= t m)
          (print (str/join "\n" out))
          (let [op (next-tok)
                p (Long/parseLong (next-tok))]
            (if (or (< (count correct) quorum) (crashed p))
              (do
                (when (= op "W") (next-tok))
                (recur (inc t) val tag (conj out "BLOCKED")))
              (if (= op "W")
                (let [v (Long/parseLong (next-tok))
                      [nv ntg line] (abd-write correct val tag p v)]
                  (recur (inc t) nv ntg (conj out line)))
                (let [[nv ntg line] (abd-read correct val tag)]
                  (recur (inc t) nv ntg (conj out line)))))))))))
