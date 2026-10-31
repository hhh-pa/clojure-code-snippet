
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

(defn legal-moves [stones k]
  (vec (for [m (range 1 (inc k)) :when (<= m stones)] m)))

(defn select-expand [tree k v]
  (loop [t tree cur v traj [v]]
    (let [nd (nth t cur)]
      (if (zero? (:stones nd))
        [t cur traj]
        (if (< (count (:kids nd)) (count (:moves nd)))
          (let [mv (nth (:moves nd) (count (:kids nd)))
                child {:stones (- (:stones nd) mv) :player (- 1 (:player nd))
                       :visits 0 :wins 0 :moves (legal-moves (- (:stones nd) mv) k)
                       :kids []}
                ni (count t)
                t2 (-> t
                       (update-in [cur :kids] conj ni)
                       (conj child))]
            [t2 ni (conj traj ni)])
          (let [lN (Math/log (double (:visits nd)))
                bi (loop [i 0 b 0 bu -1e300]
                     (if (>= i (count (:kids nd)))
                       b
                       (let [c (nth t (nth (:kids nd) i))
                             u (+ (- 1.0 (/ (double (:wins c)) (double (:visits c))))
                                    (Math/sqrt (/ (* 2.0 lN) (double (:visits c)))))]
                         (if (> u bu) (recur (inc i) i u) (recur (inc i) b bu)))))]
            (recur t (nth (:kids nd) bi) (conj traj (nth (:kids nd) bi)))))))))

(defn simulate [stones player k rng]
  (loop [st stones pl player r rng]
    (if (zero? st)
      [(- 1 pl) r]
      (let [nm (count (legal-moves st k))
            [r2 pick] (lcg-int r nm)
            mv (nth (legal-moves st k) pick)]
        (recur (- st mv) (- 1 pl) r2)))))

(defn mcts [n k iters seed]
  (loop [t [{:stones n :player 0 :visits 0 :wins 0
             :moves (legal-moves n k) :kids []}]
         rng seed i 0]
    (if (>= i iters)
      t
      (let [[t2 v traj] (select-expand t k 0)
            nd (nth t2 v)
            [winner r2] (simulate (:stones nd) (:player nd) k rng)
            t3 (reduce (fn [tt idx]
                         (-> tt
                             (update-in [idx :visits] inc)
                             (update-in [idx :wins]
                                        (fn [w] (if (= (:player (nth tt idx)) winner) (inc w) w)))))
                       t2 traj)]
        (recur t3 r2 (inc i))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (int (nth nums pos)) k (int (nth nums (+ pos 1)))
              iters (int (nth nums (+ pos 2))) seed (nth nums (+ pos 3))
              tree (mcts n k iters seed)
              rt (first tree)
              [best _] (reduce (fn [[bm bv] ci]
                                 (let [c (nth tree ci)
                                       mv (- (:stones rt) (:stones c))]
                                   (if (> (:visits c) bv) [mv (:visits c)] [bm bv])))
                               [(first (:moves rt)) -1] (:kids rt))
              vs (mapv #(str (:visits (nth tree %))) (:kids rt))
              ws (mapv #(str (:wins (nth tree %))) (:kids rt))]
          (.append sb (str "MCTS " best " V " (str/join " " vs)
                           " W " (str/join " " ws) "\n"))
          (recur (+ pos 4) (dec tcases) sb))))))

(-main)
