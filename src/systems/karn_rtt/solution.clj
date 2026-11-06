;; karn-rtt: Karn + Jacobson/Karels RTO estimator (RFC 6298 integers).
;; G=1, RTO in [1,60000], init RTO 1000. Input: q events:
;; S <rtt> <retrans> (retrans samples ignored per Karn), T (timeout
;; doubles RTO). Output per event: "SRTT RTTVAR RTO". Pure Clojure.

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
    (loop [i 0 pos 1 srtt 0 rttvar 0 rto 1000 have false out []]
      (if (>= i q) out
        (let [e (nth toks pos)]
          (cond
            (= e "S") (let [r (read-int (nth toks (+ pos 1))) ret (read-int (nth toks (+ pos 2)))]
                        (if (= ret 0)
                          (let [[s2 v2] (if have
                                          [(quot (+ (* 7 srtt) r) 8)
                                           (quot (+ (* 3 rttvar) (if (neg? (- srtt r)) (- (- srtt r)) (- srtt r))) 4)]
                                          [r (quot r 2)])
                                rto2 (min 60000 (max 1 (+ s2 (max 1 (* 4 v2)))))]
                            (recur (+ i 1) (+ pos 3) s2 v2 rto2 true (conj out (str s2 " " v2 " " rto2))))
                          (recur (+ i 1) (+ pos 3) srtt rttvar rto have (conj out (str srtt " " rttvar " " rto)))))
            (= e "T") (let [rto2 (min 60000 (* rto 2))]
                        (recur (+ i 1) (+ pos 1) srtt rttvar rto2 have (conj out (str srtt " " rttvar " " rto2))))
            :else (recur (+ i 1) (+ pos 1) srtt rttvar rto have (conj out (str srtt " " rttvar " " rto)))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
