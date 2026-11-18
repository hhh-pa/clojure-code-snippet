;; Bounded-step deterministic Turing machine simulation (Rosen 13).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          accept (Long/parseLong (nth toks 1))
          reject (Long/parseLong (nth toks 2))
          bound (Long/parseLong (nth toks 3))
          m (Long/parseLong (nth toks 4))
          trans (loop [i 0 p 5 tr {}]
                  (if (>= i m)
                    tr
                    (recur (inc i) (+ p 5)
                           (assoc tr [(Long/parseLong (nth toks p)) (nth toks (+ p 1))]
                                  [(nth toks (+ p 2)) (nth toks (+ p 3))
                                   (Long/parseLong (nth toks (+ p 4)))]))))
          tpos (+ 5 (* 5 m))
          word (if (< tpos (count toks)) (nth toks tpos) "")
          tape (into {} (map-indexed (fn [i c] [i (str c)]) word))]
      (loop [state 0 head 0 steps 0 tape tape]
        (if (>= steps bound)
          (print "TIMEOUT")
          (let [sym (get tape head ".")
                rule (get trans [state sym])]
            (if (nil? rule)
              (print (str "REJECT " steps))
              (let [[b d q2] rule
                    tape (assoc tape head b)
                    head (+ head (cond (= d "L") -1 (= d "R") 1 :else 0))
                    steps (inc steps)]
                (cond
                  (= q2 accept) (print (str "ACCEPT " steps))
                  (= q2 reject) (print (str "REJECT " steps))
                  :else (recur q2 head steps tape))))))))))
