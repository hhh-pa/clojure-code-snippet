;; peterson_election: Peterson's asynchronous unidirectional ring leader
;; election (Lynch 15.1.3). Active nodes carry an alias; per phase each
;; learns its two nearest active counter-clockwise aliases and survives iff
;; the nearest is the strict max, adopting it. Sole survivor declares.
;; Batch 6 (T322). Input: n then n ids. Output: leader/alias/phases/msgs.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0)
            ids (subvec toks 1 (+ 1 n))
            cap (+ (* 4 n) 10)]
        (loop [cur ids act (vec (repeat n true)) ph 0 lead -1 al -1]
          (let [a (filterv act (range n)) k (count a)]
            (cond
              (or (>= ph cap) (zero? k))
              (do (println (str "LEADER: " lead)) (println (str "ALIAS: " al))
                  (println (str "PHASES: " ph)) (println (str "MESSAGES: " (* 2 n ph))))
              (= k 1)
              (let [q (nth a 0) ph2 (inc ph)]
                (println (str "LEADER: " (nth ids q))) (println (str "ALIAS: " (nth cur q)))
                (println (str "PHASES: " ph2)) (println (str "MESSAGES: " (* 2 n ph2))))
              :else
              (let [res (reduce (fn [[nc na done l al2] j]
                                  (if done [nc na done l al2]
                                    (let [q (nth a j)
                                          u2 (nth cur (nth a (mod (- j 1) k)))
                                          u3 (nth cur (nth a (mod (- j 2) k)))
                                          cq (nth cur q)]
                                      (cond (= u2 cq) [nc na true (nth ids q) cq]
                                            (and (> u2 cq) (> u2 u3)) [(assoc nc q u2) na false l al2]
                                            :else [nc (assoc na q false) false l al2]))))
                                [cur act false lead al] (range k))
                    [nc na done l al2] res]
                (if done
                  (let [ph2 (inc ph)]
                    (println (str "LEADER: " l)) (println (str "ALIAS: " al2))
                    (println (str "PHASES: " ph2)) (println (str "MESSAGES: " (* 2 n ph2))))
                  (recur nc na (inc ph) lead al))))))))))
(-main (slurp *in*))
