;; ben_or: Ben-Or randomized asynchronous binary consensus (lockstep
;; simulation). Stage 1 proposes v on strict majority (>n/2) of correct
;; votes, else abstains; stage 2 decides v if >=f+1 propose it unopposed,
;; adopts v if proposed but short of quorum, else each correct process
;; flips a coin (fixed-seed LCG). Batch 6 (T342).
;; Input: n f Rmax, binary inputs, faulty list.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn lcg-next [x] (unchecked-add (unchecked-multiply x 6364136223846793005) 1442695040888963407))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [t (mapv parse-token toks)
            n (nth t 0) f (nth t 1) rmax (nth t 2)
            x0 (subvec t 3 (+ 3 n))
            k (nth t (+ 3 n))
            fl (subvec t (+ 4 n) (+ 4 n k))
            faulty (reduce (fn [a q] (assoc a q true)) (vec (repeat n false)) fl)
            correct (filterv (fn [i] (not (nth faulty i))) (range n))
            nc (count correct)]
        (loop [r 1 x x0 lcg 777 flips 0]
          (if (> r rmax)
            (do (println "DECISION: UNDECIDED") (println (str "ROUNDS: " rmax)) (println (str "COINFLIPS: " flips)))
            (let [o (count (filter #(= (nth x %) 1) correct))
                  z (- nc o)
                  prop (cond (> o (quot n 2)) 1 (> z (quot n 2)) 0 :else -1)]
              (cond
                (and (>= prop 0) (>= nc (+ f 1)))
                (do (println (str "DECISION: " prop)) (println (str "ROUNDS: " r)) (println (str "COINFLIPS: " flips)))
                (>= prop 0)
                (recur (inc r) (reduce (fn [a i] (assoc a i prop)) x correct) lcg flips)
                :else
                (let [[x2 l2 f2] (reduce (fn [[a l c] i] (let [d (lcg-next l)]
                                                           [(assoc a i (bit-and (unsigned-bit-shift-right d 33) 1)) d (inc c)]))
                                          [x lcg flips] correct)]
                  (recur (inc r) x2 l2 f2))))))))))
(-main (slurp *in*))
