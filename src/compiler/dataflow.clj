
;; T1206 Batch 5 - 314 dataflow : LIVE / REACH / AVAIL iterative analysis.
;; Input: MODE nblocks; AVAIL expr table; per-block succ/def/use-or-gen lists.
;; Output: per-block IN/OUT sets + ITERATIONS.
(ns dataflow
  (:require [clojure.string :as str]))
(def separators #{\space \tab \return})
(defn digit-values [ch] (- (int ch) (int \0)))
(defn tokenize [line]
  (loop [i 0 cur "" toks []]
    (if (>= i (count line))
      (if (empty? cur) toks (conj toks cur))
      (let [c (.charAt line i)]
        (if (contains? separators c)
          (recur (inc i) "" (if (empty? cur) toks (conj toks cur)))
          (recur (inc i) (str cur c) toks))))))
(defn parse-token [s]
  (loop [i 0 v 0]
    (if (>= i (count s)) v (recur (inc i) (+ (* v 10) (digit-values (.charAt s i)))))))
(defn parse-nums [toks p k]
  (loop [i 0 v [] pp p]
    (if (>= i k) [v pp]
      (recur (inc i) (conj v (parse-token (nth toks pp))) (inc pp)))))
(defn parse-words [toks p k]
  (loop [i 0 v [] pp p]
    (if (>= i k) [v pp]
      (recur (inc i) (conj v (nth toks pp)) (inc pp)))))
(defn idx-of [names v]
  (loop [i 0]
    (if (= (nth names i) v) i (recur (inc i)))))
(defn bits-of [names vs]
  (loop [i 0 m 0]
    (if (>= i (count vs)) m
      (recur (inc i) (bit-or m (bit-shift-left 1 (idx-of names (nth vs i))))))))
(defn -main []
  (let [toks (vec (mapcat tokenize (str/split-lines (slurp *in*))))
        mode (nth toks 0)
        n (parse-token (nth toks 1))
        [p1 exId exV1 exV2]
        (if (= mode "AVAIL")
          (let [ne (parse-token (nth toks 2))]
            (loop [i 0 pp 3 a [] b [] c []]
              (if (>= i ne) [pp a b c]
                (recur (inc i) (+ pp 4) (conj a (nth toks pp))
                       (conj b (nth toks (+ pp 1))) (conj c (nth toks (+ pp 3)))))))
          [2 [] [] []])
        [succ defs uses gens allvars]
        (loop [b 0 pp p1 s [] d [] u [] g [] av []]
          (if (>= b n) [s d u g av]
            (let [ns (parse-token (nth toks pp))
                  [sv p2] (parse-nums toks (inc pp) ns)
                  nd (parse-token (nth toks p2))
                  [dv p3] (parse-words toks (inc p2) nd)
                  nk (parse-token (nth toks p3))
                  [kv p4] (parse-words toks (inc p3) nk)]
              (recur (inc b) p4 (conj s sv) (conj d dv)
                     (if (= mode "AVAIL") u (conj u kv))
                     (if (= mode "AVAIL") (conj g kv) g)
                     (into (into av (vec dv)) (if (= mode "AVAIL") [] (vec kv)))))))
        pred (vec (map (fn [s] (vec (filter (fn [b] (some #(= % s) (nth succ b))) (range n)))) (range n)))
        dvar (vec (apply concat defs))
        dids (loop [b 0 off 0 dd []]
               (if (>= b n) dd
                 (recur (inc b) (+ off (count (nth defs b))) (conj dd (vec (range off (+ off (count (nth defs b)))))))))
        names (cond (= mode "LIVE") (vec (sort (set allvars)))
                    (= mode "REACH") (vec (map #(str "d" %) (range (count dvar))))
                    :else exId)
        U (count names)
        DF (loop [b 0 v (vec (repeat n 0))]
             (if (>= b n) v
               (recur (inc b) (assoc v b (if (= mode "LIVE") (bits-of names (nth defs b)) 0)))))
        US (loop [b 0 v (vec (repeat n 0))]
             (if (>= b n) v
               (recur (inc b) (assoc v b (if (= mode "LIVE") (bits-of names (nth uses b)) 0)))))
        GN (loop [b 0 v (vec (repeat n 0))]
             (if (>= b n) v
               (recur (inc b) (assoc v b (cond (= mode "REACH")
                                               (loop [i 0 m 0]
                                                 (if (>= i (count (nth dids b))) m
                                                   (recur (inc i) (bit-or m (bit-shift-left 1 (nth (nth dids b) i))))))
                                               (= mode "AVAIL") (bits-of names (nth gens b))
                                               :else 0)))))
        KL (loop [b 0 v (vec (repeat n 0))]
             (if (>= b n) v
               (recur (inc b) (assoc v b (cond (= mode "REACH")
                                               (loop [i 0 m 0]
                                                 (if (>= i U) m
                                                   (recur (inc i) (if (and (not (some #(= % i) (nth dids b)))
                                                                            (some #(= % (nth dvar i)) (nth defs b)))
                                                                      (bit-or m (bit-shift-left 1 i)) m))))
                                               (= mode "AVAIL")
                                               (loop [i 0 m 0]
                                                 (if (>= i U) m
                                                   (recur (inc i) (if (some #(or (= % (nth exV1 i)) (= % (nth exV2 i))) (nth defs b))
                                                                      (bit-or m (bit-shift-left 1 i)) m))))
                                               :else 0)))))
        ALL (if (zero? U) 0 (dec (bit-shift-left 1 U)))
        masknot (fn [x] (bit-and ALL (bit-not x)))
        [IN OUT iters]
        (loop [IN (vec (repeat n 0)) OUT (vec (repeat n (if (= mode "AVAIL") ALL 0))) iters 0]
          (let [order (if (= mode "LIVE") (vec (reverse (range n))) (vec (range n)))
                [IN2 OUT2 ch]
                (loop [oi 0 INc IN OUTc OUT ch false]
                  (if (>= oi n) [INc OUTc ch]
                    (let [b (nth order oi)]
                      (if (= mode "LIVE")
                        (let [o (loop [i 0 m 0]
                                                  (if (>= i (count (nth succ b))) m
                                                    (recur (inc i) (bit-or m (nth INc (nth (nth succ b) i))))))
                              ni (bit-and ALL (bit-or (nth US b) (bit-and o (masknot (nth DF b)))))]
                          (if (or (not= o (nth OUTc b)) (not= ni (nth INc b)))
                            (recur (inc oi) (assoc INc b ni) (assoc OUTc b o) true)
                            (recur (inc oi) INc OUTc ch)))
                        (let [ni (cond (= mode "REACH")
                                       (if (= b 0) 0
                                         (loop [i 0 m 0]
                                           (if (>= i (count (nth pred b))) m
                                             (recur (inc i) (bit-or m (nth OUTc (nth (nth pred b) i)))))))
                                       (= b 0) 0
                                       (empty? (nth pred b)) ALL
                                       :else (loop [i 0 m ALL]
                                               (if (>= i (count (nth pred b))) m
                                                 (recur (inc i) (bit-and m (nth OUTc (nth (nth pred b) i)))))))
                              no (bit-and ALL (bit-or (nth GN b) (bit-and ni (masknot (nth KL b)))))]
                          (if (or (not= ni (nth INc b)) (not= no (nth OUTc b)))
                            (recur (inc oi) (assoc INc b ni) (assoc OUTc b no) true)
                            (recur (inc oi) INc OUTc ch)))))))]
            (if ch (recur IN2 OUT2 (inc iters)) [IN2 OUT2 (inc iters)])))
        sb (StringBuilder.)]
    (loop [b 0]
      (when (< b n)
        (.append sb (str "B" b " IN={"))
        (loop [i 0 f true]
          (when (< i U)
            (if (zero? (bit-and (bit-shift-right (nth IN b) i) 1))
              (recur (inc i) f)
              (do (when (not f) (.append sb ","))
                  (.append sb (nth names i))
                  (recur (inc i) false)))))
        (.append sb "} OUT={")
        (loop [i 0 f true]
          (when (< i U)
            (if (zero? (bit-and (bit-shift-right (nth OUT b) i) 1))
              (recur (inc i) f)
              (do (when (not f) (.append sb ","))
                  (.append sb (nth names i))
                  (recur (inc i) false)))))
        (.append sb "}\n")
        (recur (inc b))))
    (.append sb (str "ITERATIONS " iters "\n"))
    (print (str sb))))
(-main)
