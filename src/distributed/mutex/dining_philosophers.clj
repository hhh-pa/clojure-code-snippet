;; dining_philosophers: Lehmann-Rabin randomized dining philosophers
;; (RightLeftDP). Per round, thinking philosophers get hungry (p=0.6,
;; fixed-seed LCG); hungry ones draw left/right-first and grab the first
;; fork if free; holders take the second if free (else release the first
;; and retry next round); eating holds both forks one full round.
;; Deadlock-free (no hold-and-wait across rounds). Batch 6 (T335).
;; Input: n R. Forks: phil i uses left=i, right=(i+1)%n.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn lcg-next [x] (unchecked-add (unchecked-multiply x 6364136223846793005) 1442695040888963407))
(defn urem [x m] (Long/remainderUnsigned x m))
(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (tokenize input)
          n (Long/parseLong (nth toks 0)) R (Long/parseLong (nth toks 1))]
      (loop [r 0 lcg 12345 fork (vec (repeat n 0)) hun (vec (repeat n false))
             held (vec (repeat n -1)) est (vec (repeat n 0))
             eats (vec (repeat n 0)) waits (vec (repeat n 0))]
        (if (>= r R)
          (do (doseq [i (range n)] (println (str "P" i ": eats=" (nth eats i) " waits=" (nth waits i))))
              (println (str "TOTALEATS: " (reduce + eats)))
              (println (str "STUCK: " (count (filterv #(not= % 0) fork)))))
          (let [[l2 hun2 held2]
                (loop [i 0 l lcg h hun hd held]
                  (if (>= i n) [l h hd]
                    (if (or (nth h i) (not= (nth est i) 0)) (recur (inc i) l h hd)
                      (let [d (lcg-next l)]
                        (if (< (urem (unsigned-bit-shift-right d 16) 100) 60)
                          (recur (inc i) d (assoc h i true) (assoc hd i -1))
                          (recur (inc i) d h hd))))))
                [l3 fork3 held3]
                (loop [i 0 l l2 fk fork hd held2]
                  (if (>= i n) [l fk hd]
                    (if (or (not (nth hun2 i)) (not= (nth hd i) -1)) (recur (inc i) l fk hd)
                      (let [d (lcg-next l)
                            f1 (if (zero? (bit-and (unsigned-bit-shift-right d 33) 1)) i (mod (inc i) n))]
                        (if (zero? (nth fk f1))
                          (recur (inc i) d (assoc fk f1 (inc i)) (assoc hd i f1))
                          (recur (inc i) d fk hd))))))
                [fork4 held4 est4]
                (loop [i 0 fk fork3 hd held3 e est]
                  (if (>= i n) [fk hd e]
                    (if (or (not (nth hun2 i)) (= (nth hd i) -1) (not= (nth e i) 0))
                      (recur (inc i) fk hd e)
                      (let [lf i rt (mod (inc i) n)
                            f2 (if (= (nth hd i) lf) rt lf)]
                        (if (zero? (nth fk f2))
                          (recur (inc i) (assoc fk f2 (inc i)) hd (assoc e i 1))
                          (recur (inc i) (assoc fk (nth hd i) 0) (assoc hd i -1) e))))))
                res (loop [i 0 fk fork4 hd held4 e est4 h hun2 ea eats wa waits]
                      (if (>= i n) [fk hd e h ea wa]
                        (let [ei (nth e i)]
                          (cond (= ei 2) (recur (inc i) (assoc (assoc fk i 0) (mod (inc i) n) 0)
                                               hd (assoc e i 0) (assoc h i false)
                                               (assoc ea i (inc (nth ea i))) wa)
                                (= ei 1) (recur (inc i) fk hd (assoc e i 2) h ea wa)
                                (nth h i) (recur (inc i) fk hd e h ea (assoc wa i (inc (nth wa i))))
                                :else (recur (inc i) fk hd e h ea wa)))))
                [fk5 hd5 e5 h5 ea5 wa5] res]
            (recur (inc r) l3 fk5 h5 hd5 e5 ea5 wa5)))))))
(-main (slurp *in*))
