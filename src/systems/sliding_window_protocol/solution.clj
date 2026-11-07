;; sliding-window-protocols: Stop-and-Wait, Go-Back-N, Selective Repeat.
;; Discrete-time simulation, RTT=2, TIMEOUT=4. Input: policy W
;; (0 SW, 1 GBN, 2 SR); N L; L loss lines "F s"/"A s" (each consumed
;; once). Output: "<frame_tx> <ack_tx>". Pure Clojure.

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
  (when (and (string? tok) (pos? (count tok)))
    (parse-long tok)))

(defn read-int [tok] (parse-token tok))

(def TIMEOUT 4)
(def TMAX 90000)
(def TSIZE 100000)

(defn solve [toks]
  (let [policy (read-int (nth toks 0))
        W (if (= policy 0) 1 (read-int (nth toks 1)))
        N (read-int (nth toks 2))
        L (read-int (nth toks 3))
        floss (transient (vec (repeat N 0)))
        aloss (transient (vec (repeat N 0)))
        _ (loop [i 0]
            (when (< i L)
              (let [ty (nth toks (+ 4 (* i 2)))
                    s (read-int (nth toks (+ 5 (* i 2))))]
                (when (and (>= s 0) (< s N))
                  (if (= ty "F")
                    (assoc! floss s (inc (nth floss s)))
                    (assoc! aloss s (inc (nth aloss s)))))
                (recur (inc i)))))
        sent (transient (vec (repeat N -1000000)))
        acked (transient (vec (repeat N false)))
        buffered (transient (vec (repeat N false)))]
    (loop [now 0
           base 0
           next-seq 0
           expected 0
           frame-tx 0
           ack-tx 0
           cur-farr []
           cur-aarr []]
      (if (>= now TMAX)
        (str frame-tx " " ack-tx)
        (let [nf (count cur-farr)
              [expected ack-tx next-aarr]
              (loop [idx 0 exp expected atx ack-tx na []]
                (if (>= idx nf)
                  [exp atx na]
                  (let [j (nth cur-farr idx)]
                    (if (or (< j 0) (>= j N))
                      (recur (inc idx) exp atx na)
                      (if (= policy 2)
                        (let [exp2 (cond
                                     (= j exp)
                                     (loop [e (inc exp)]
                                       (if (and (< e N) (nth buffered e))
                                         (do (assoc! buffered e false)
                                             (recur (inc e)))
                                         e))
                                     (and (> j exp) (< j (+ exp W)))
                                     (do (assoc! buffered j true) exp)
                                     :else exp)
                              al (nth aloss j)]
                          (if (pos? al)
                            (do (assoc! aloss j (dec al))
                                (recur (inc idx) exp2 (inc atx) na))
                            (recur (inc idx) exp2 (inc atx) (conj na j))))
                        (let [exp2 (if (= j exp) (inc exp) exp)]
                          (if (pos? exp2)
                            (let [aj (dec exp2)
                                  al (nth aloss aj)]
                              (if (pos? al)
                                (do (assoc! aloss aj (dec al))
                                    (recur (inc idx) exp2 (inc atx) na))
                                (recur (inc idx) exp2 (inc atx) (conj na aj))))
                            (recur (inc idx) exp2 atx na))))))))
              na-cnt (count cur-aarr)
              _ (loop [idx 0]
                  (when (< idx na-cnt)
                    (let [j (nth cur-aarr idx)]
                      (if (= policy 2)
                        (when (and (>= j 0) (< j N))
                          (assoc! acked j true))
                        (let [hi (min j (dec N))]
                          (loop [k base]
                            (when (<= k hi)
                              (assoc! acked k true)
                              (recur (inc k)))))))
                    (recur (inc idx))))
              base2 (loop [b base]
                      (if (and (< b N) (nth acked b))
                        (recur (inc b))
                        b))]
          (if (>= base2 N)
            (str frame-tx " " ack-tx)
            (let [[next-seq2 frame-tx2 next-farr]
                  (if (= policy 2)
                    (loop [j base2 ftx frame-tx nf-arr []]
                      (if (>= j next-seq)
                        [next-seq ftx nf-arr]
                        (if (and (not (nth acked j)) (>= (- now (nth sent j)) TIMEOUT))
                          (let [fl (nth floss j)]
                            (assoc! sent j now)
                            (if (pos? fl)
                              (do (assoc! floss j (dec fl))
                                  (recur (inc j) (inc ftx) nf-arr))
                              (recur (inc j) (inc ftx) (conj nf-arr j))))
                          (recur (inc j) ftx nf-arr))))
                    [(if (and (< base2 next-seq) (>= (- now (nth sent base2)) TIMEOUT))
                       base2
                       next-seq)
                     frame-tx
                     []])
                  lim (min (+ base2 W) N)
                  [next-seq3 frame-tx3 next-farr2]
                  (loop [ns next-seq2 ftx frame-tx2 nf-arr next-farr]
                    (if (>= ns lim)
                      [ns ftx nf-arr]
                      (let [fl (nth floss ns)]
                        (assoc! sent ns now)
                        (if (pos? fl)
                          (do (assoc! floss ns (dec fl))
                              (recur (inc ns) (inc ftx) nf-arr))
                          (recur (inc ns) (inc ftx) (conj nf-arr ns))))))]
              (recur (inc now) base2 next-seq3 expected frame-tx3 ack-tx next-farr2 next-aarr))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
