;; sliding-window-protocols: Stop-and-Wait, Go-Back-N, Selective Repeat.
;; Discrete-time simulation, RTT=2, TIMEOUT=4. Input: policy W
;; (0 SW, 1 GBN, 2 SR); N L; L loss lines "F s"/"A s" (each consumed
;; once). Output: "<frame_tx> <ack_tx>". Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
                           {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(def TIMEOUT 4)
(def TMAX 90000)
(def TSIZE 100000)

(defn send-frame [st j now]
  (let [e (get (:floss st) j 0)]
    (if (pos? e)
      (assoc st :frame-tx (+ (:frame-tx st) 1) :sent (assoc (:sent st) j now) :floss (assoc (:floss st) j (- e 1)))
      (assoc st :frame-tx (+ (:frame-tx st) 1) :sent (assoc (:sent st) j now)
             :farr (assoc (:farr st) (+ now 1) (conj (nth (:farr st) (+ now 1)) j))))))

(defn send-ack [st j now]
  (let [e (get (:aloss st) j 0)]
    (if (pos? e)
      (assoc st :ack-tx (+ (:ack-tx st) 1) :aloss (assoc (:aloss st) j (- e 1)))
      (assoc st :ack-tx (+ (:ack-tx st) 1)
             :aarr (assoc (:aarr st) (+ now 1) (conj (nth (:aarr st) (+ now 1)) j))))))

(defn solve [toks]
  (let [policy (read-int (nth toks 0))
        W (if (= policy 0) 1 (read-int (nth toks 1)))
        N (read-int (nth toks 2)) L (read-int (nth toks 3))
        [floss aloss] (loop [i 0 fl {} al {}]
                        (if (>= i L) [fl al]
                          (let [ty (nth toks (+ 4 (* i 2))) s (read-int (nth toks (+ 5 (* i 2))))]
                            (if (= ty "F")
                              (recur (+ i 1) (assoc fl s (+ (get fl s 0) 1)) al)
                              (recur (+ i 1) fl (assoc al s (+ (get al s 0) 1)))))))
        init {:base 0 :next 0 :sent (vec (repeat N -1000000)) :acked (vec (repeat N false))
              :expected 0 :buffered (vec (repeat N false))
              :farr (vec (repeat TSIZE [])) :aarr (vec (repeat TSIZE []))
              :floss floss :aloss aloss :frame-tx 0 :ack-tx 0}]
    (loop [now 0 st init]
      (if (>= now TMAX) (str (:frame-tx st) " " (:ack-tx st))
        (let [st1 (loop [fs (nth (:farr st) now) st2 st]
                    (if (empty? fs) st2
                      (let [j (first fs) more (rest fs)]
                        (if (or (< j 0) (>= j N)) (recur more st2)
                          (if (= policy 2)
                            (let [exp (:expected st2)
                                  st3 (cond (= j exp)
                                            (let [e2 (+ exp 1)
                                                  [e3 bf3] (loop [e e2 bf (:buffered st2)]
                                                             (if (and (< e N) (nth bf e)) (recur (+ e 1) (assoc bf e false)) [e bf]))]
                                              (assoc st2 :expected e3 :buffered bf3))
                                            (and (> j exp) (< j (+ exp W)))
                                            (assoc st2 :buffered (assoc (:buffered st2) j true))
                                            :else st2)]
                              (recur more (send-ack st3 j now)))
                            (let [st3 (if (= j (:expected st2)) (assoc st2 :expected (+ (:expected st2) 1)) st2)]
                              (recur more (if (pos? (:expected st3)) (send-ack st3 (- (:expected st3) 1) now) st3))))))))
              st2 (loop [as (nth (:aarr st1) now) st3 st1]
                    (if (empty? as) st3
                      (let [j (first as) more (rest as)]
                        (recur more
                               (if (= policy 2)
                                 (if (and (>= j 0) (< j N)) (assoc st3 :acked (assoc (:acked st3) j true)) st3)
                                 (assoc st3 :acked (loop [k (:base st3) ak (:acked st3)]
                                                     (if (or (>= k N) (> k j)) ak
                                                       (recur (+ k 1) (assoc ak k true))))))))))
              base2 (loop [b (:base st2)] (if (and (< b N) (nth (:acked st2) b)) (recur (+ b 1)) b))
              st3 (assoc st2 :base base2)]
          (if (>= base2 N) (str (:frame-tx st3) " " (:ack-tx st3))
            (let [st4 (if (= policy 2)
                        (loop [j base2 st5 st3]
                          (if (>= j (:next st5)) st5
                            (recur (+ j 1)
                                   (if (and (not (nth (:acked st5) j)) (>= (- now (nth (:sent st5) j)) TIMEOUT))
                                     (send-frame st5 j now) st5))))
                        (if (and (< base2 (:next st3)) (>= (- now (nth (:sent st3) base2)) TIMEOUT))
                          (assoc st3 :next base2) st3))
                  st5 (loop [st6 st4]
                        (if (and (< (:next st6) (+ (:base st6) W)) (< (:next st6) N))
                          (recur (let [st7 (send-frame st6 (:next st6) now)]
                                   (assoc st7 :next (+ (:next st7) 1))))
                          st6))]
              (recur (+ now 1) st5))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
