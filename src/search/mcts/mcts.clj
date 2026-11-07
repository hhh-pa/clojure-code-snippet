
;; mcts: Monte Carlo Tree Search with UCB1 on the take-away game (heap of
;; N stones, take 1..K). Seeded LCG playouts; wins counted from each
;; node's player perspective; best root move by visits.
(ns mcts
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(def ^:private lcg-mult 6364136223846793005)
(def ^:private lcg-inc 1442695040888963407)

(defn lcg-next [s] (unchecked-add (unchecked-multiply s lcg-mult) lcg-inc))
(defn lcg-int [s n]
  (let [ns (lcg-next s)] [ns (int (Long/remainderUnsigned ns n))]))

(defn simulate [stones player k rng]
  (loop [st stones pl player r rng]
    (if (zero? st)
      [(- 1 pl) r]
      (let [nm (min st k)
            r2 (lcg-next r)
            pick (int (Long/remainderUnsigned r2 nm))
            mv (inc pick)]
        (recur (- st mv) (- 1 pl) r2)))))

(defn mcts [n k iters seed]
  (let [stones-v (transient [n])
        player-v (transient [0])
        visits-v (transient [0])
        wins-v (transient [0])
        kids-v (transient [[]])]
    (loop [rng seed i 0]
      (if (>= i iters)
        [(persistent! stones-v)
         (persistent! visits-v)
         (persistent! wins-v)
         (persistent! kids-v)]
        (let [[v traj] (loop [cur 0 tr [0]]
                         (let [st (nth stones-v cur)]
                           (if (zero? st)
                             [cur tr]
                             (let [kds (nth kids-v cur)
                                   nk (count kds)
                                   nm (min st k)]
                               (if (< nk nm)
                                 (let [mv (inc nk)
                                       ni (count stones-v)]
                                   (conj! stones-v (- st mv))
                                   (conj! player-v (- 1 (nth player-v cur)))
                                   (conj! visits-v 0)
                                   (conj! wins-v 0)
                                   (conj! kids-v [])
                                   (assoc! kids-v cur (conj kds ni))
                                   [ni (conj tr ni)])
                                 (let [lN (Math/log (double (nth visits-v cur)))
                                       bi (loop [j 0 b 0 bu -1e300]
                                            (if (>= j nk)
                                              b
                                              (let [ci (nth kds j)
                                                    cv (double (nth visits-v ci))
                                                    cw (double (nth wins-v ci))
                                                    u (+ (- 1.0 (/ cw cv))
                                                         (Math/sqrt (/ (* 2.0 lN) cv)))]
                                                (if (> u bu)
                                                  (recur (inc j) j u)
                                                  (recur (inc j) b bu)))))
                                       nxt (nth kds bi)]
                                   (recur nxt (conj tr nxt))))))))
              [winner r2] (simulate (nth stones-v v) (nth player-v v) k rng)
              nt (count traj)]
          (loop [j 0]
            (when (< j nt)
              (let [idx (nth traj j)]
                (assoc! visits-v idx (inc (nth visits-v idx)))
                (when (= (nth player-v idx) winner)
                  (assoc! wins-v idx (inc (nth wins-v idx)))))
              (recur (inc j))))
          (recur r2 (inc i)))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (int (nth nums pos)) k (int (nth nums (+ pos 1)))
              iters (int (nth nums (+ pos 2))) seed (nth nums (+ pos 3))
              [stones-v visits-v wins-v kids-v] (mcts n k iters seed)
              rt-stones (nth stones-v 0)
              rt-kids (nth kids-v 0)
              [best _] (reduce (fn [[bm bv] ci]
                                 (let [cv (nth visits-v ci)
                                       mv (- rt-stones (nth stones-v ci))]
                                   (if (> cv bv) [mv cv] [bm bv])))
                               [1 -1] rt-kids)
              vs (mapv #(str (nth visits-v %)) rt-kids)
              ws (mapv #(str (nth wins-v %)) rt-kids)]
          (.append sb (str "MCTS " best " V " (str/join " " vs)
                           " W " (str/join " " ws) "\n"))
          (recur (+ pos 4) (dec tcases) sb))))))

(-main)
