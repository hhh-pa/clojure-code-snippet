;; tcp-congestion: slow start, AIMD, fast retransmit/recovery (Reno-lite).
;; Integer-exact (MSS=1; CA grows 1 MSS per cwnd ACKs).
;; Input: q; q events A (new ACK), D (3 dupACKs), T (timeout).
;; Output per event: "cwnd ssthresh". Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-int [tok] (parse-token tok))

(defn solve [toks]
  (let [q (read-int (nth toks 0))]
    (loop [i 0 cwnd 1 ssth 64 ackc 0 out []]
      (if (>= i q) out
        (let [e (nth toks (+ 1 i))
              [cw2 ss2 ac2] (cond
                              (= e "A") (if (< cwnd ssth) [(+ cwnd 1) ssth ackc]
                                          (let [a2 (+ ackc 1)]
                                            (if (>= a2 cwnd) [(+ cwnd 1) ssth 0] [cwnd ssth a2])))
                              (= e "D") (let [s2 (max (quot cwnd 2) 1)] [s2 s2 0])
                              (= e "T") (let [s2 (max (quot cwnd 2) 1)] [1 s2 0])
                              :else [cwnd ssth ackc])]
          (recur (+ i 1) cw2 ss2 ac2 (conj out (str cw2 " " ss2))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
