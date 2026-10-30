;; quine-mccluskey: Quine-McCluskey boolean function minimization.
;; Input: nvars, minterms. Output: #primes, cover size, cover implicants.
(ns quine-mccluskey (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn bits-of [m n]
  (apply str (map (fn [i] (if (zero? (bit-and (bit-shift-right m i) 1)) \0 \1)) (range (dec n) -1 -1))))
(defn combine-pos [a b]
  (loop [i 0 diff -1]
    (if (>= i (count a)) diff
      (if (= (nth a i) (nth b i)) (recur (inc i) diff)
        (if (or (= \- (nth a i)) (= \- (nth b i)) (>= diff 0)) -1 (recur (inc i) i))))))
(defn covers? [imp mt]
  (loop [i 0]
    (if (>= i (count imp)) true
      (if (or (= \- (nth imp i)) (= (nth imp i) (nth mt i))) (recur (inc i)) false))))
(defn qm-primes [n cur]
  (loop [cur (sort cur) primes []]
    (let [groups (loop [i 0 g (vec (repeat (inc n) []))]
                   (if (>= i (count cur)) g
                     (let [t (nth cur i) c (count (filter #(= \1 %) t))]
                       (recur (inc i) (assoc g c (conj (nth g c) t))))))
          res (loop [g 0 used #{} nxt #{}]
                (if (>= g n) [used nxt]
                  (let [[used nxt]
                        (loop [as (nth groups g) used used nxt nxt]
                          (if (empty? as) [used nxt]
                            (let [[used nxt]
                                  (loop [bs (nth groups (inc g)) used used nxt nxt]
                                    (if (empty? bs) [used nxt]
                                      (let [d (combine-pos (first as) (first bs))]
                                        (if (neg? d) (recur (rest bs) used nxt)
                                          (recur (rest bs) (conj used (first as) (first bs))
                                                 (conj nxt (str (subs (first as) 0 d) "-" (subs (first as) (inc d)))))))))]
                              (recur (rest as) used nxt))))]
                    (recur (inc g) used nxt))))
          used (nth res 0) nxt (nth res 1)
          primes2 (vec (concat primes (filter #(not (used %)) cur)))]
      (if (empty? nxt) (sort (set primes2))
        (recur (sort nxt) primes2)))))
(defn qm-cover [n mts primes mtbits]
  (let [ess (set (mapcat (fn [i] (let [cov (filter #(covers? % (nth mtbits i)) primes)]
                                   (when (= 1 (count cov)) cov))) (range (count mts))))]
    (loop [cover ess]
      (let [unc (vec (filter (fn [i] (not (some #(covers? % (nth mtbits i)) cover))) (range (count mts))))]
        (if (empty? unc) (sort cover)
          (let [best (loop [ps primes best nil bc -1]
                       (if (empty? ps) best
                         (let [p (first ps)
                               c (count (filter #(covers? p (nth mtbits %)) unc))]
                           (if (or (> c bc) (and (= c bc) (or (nil? best) (neg? (compare p best)))))
                             (recur (rest ps) p c)
                             (recur (rest ps) best bc)))))]
            (recur (conj cover best))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            n (nth vals 0) k (nth vals 1)
            mts (sort (set (take k (drop 2 vals))))
            cur (mapv #(bits-of % n) mts)
            primes (qm-primes n cur)
            mtbits (mapv #(bits-of % n) mts)
            cover (qm-cover n mts primes mtbits)]
        (println (count primes))
        (println (count cover))
        (doseq [p cover] (println p))))))
(-main)
