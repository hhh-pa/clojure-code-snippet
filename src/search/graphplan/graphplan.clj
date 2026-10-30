
;; graphplan: Graphplan planning-graph construction with mutex extraction
;; plus backward solution extraction with nogood memo. No-op persistence
;; actions are explicit; plans list real actions per level.
(ns graphplan
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn bits-of [p m]
  (vec (for [x (range p) :when (= 1 (bit-and (bit-shift-right m x) 1))] x)))

(declare extract*)

(defn cover* [st k unc chosen nextmask]
  (if (empty? unc)
    (let [[ok sub] (extract* st (dec k) (bits-of (:p st) nextmask))]
      (if ok [true (conj sub (vec chosen))] [false nil]))
    (let [g (first unc)
          ach (filterv #(= 1 (bit-and (bit-shift-right (nth (:add st) %) g) 1))
                       (nth @(:actids st) (dec k)))]
      (loop [as ach]
        (if (empty? as)
          [false nil]
          (let [a (first as)
                mx (some #(get-in @(:amut st) [(dec k) (min a %) (max a %)]) chosen)]
            (if mx
              (recur (rest as))
              (let [nu (filterv #(zero? (bit-and (bit-shift-right (nth (:add st) a) %) 1)) unc)
                    [ok plan] (cover* st k nu (conj chosen a)
                                      (bit-or nextmask (nth (:pre st) a)))]
                (if ok [true plan] (recur (rest as)))))))))))

(defn extract* [st k goals]
  (if (zero? k)
    (if (every? #(= 1 (bit-and (bit-shift-right (:initmask st) %) 1)) goals)
      [true []]
      [false nil])
    (let [gm (reduce bit-or 0 (map #(bit-shift-left 1 %) goals))
          key (+ (* k 4294967296) gm)]
      (if (contains? @(:nogood st) key)
        [false nil]
        (let [[ok plan] (cover* st k goals [] 0)]
          (when (not ok) (swap! (:nogood st) conj key))
          [ok plan])))))

(defn expand-once [st l]
  (let [p (:p st) n (:n st)
        pmode (nth @(:pmodes st) l)
        aids (filterv #(zero? (bit-and (nth (:pre st) %) (bit-not pmode))) (range n))]
    (let [am0 (vec (repeat n (vec (repeat n false))))
          am (reduce (fn [m [x y]]
                       (let [mx (or (pos? (bit-and (nth (:add st) x) (nth (:del st) y)))
                                      (pos? (bit-and (nth (:add st) y) (nth (:del st) x)))
                                      (pos? (bit-and (nth (:del st) x) (nth (:pre st) y)))
                                      (pos? (bit-and (nth (:del st) y) (nth (:pre st) x)))
                                      (boolean (some (fn [[u v]] (get-in @(:pmut st) [l u v]))
                                                     (for [u (bits-of p (nth (:pre st) x))
                                                           v (bits-of p (nth (:pre st) y))] [u v]))))]
                         (-> m (assoc-in [x y] mx) (assoc-in [y x] mx))))
                     am0
                     (for [i (range (count aids)) j (range (inc i) (count aids))]
                       [(nth aids i) (nth aids j)]))]
      (swap! (:actids st) conj aids)
      (swap! (:amut st) conj am)
      (let [nmask (reduce bit-or 0 (map #(nth (:add st) %) aids))
            pm0 (vec (repeat p (vec (repeat p false))))
            [pm mc] (reduce (fn [[m c] [x y]]
                              (if (or (zero? (bit-and (bit-shift-right nmask x) 1))
                                      (zero? (bit-and (bit-shift-right nmask y) 1)))
                                [m c]
                                (let [mx (every? (fn [[u v]]
                                                   (and (not= u v) (get-in am [u v])))
                                                 (for [u aids :when (= 1 (bit-and (bit-shift-right (nth (:add st) u) x) 1))
                                                       v aids :when (= 1 (bit-and (bit-shift-right (nth (:add st) v) y) 1))]
                                                   [u v]))]
                                  [(-> m (assoc-in [x y] mx) (assoc-in [y x] mx)) (if mx (inc c) c)])))
                            [pm0 0]
                            (for [x (range p) y (range (inc x) p)] [x y]))
            prevmc (count (for [x (range p) y (range (inc x) p)
                                :when (get-in @(:pmut st) [l x y])] 1))]
        (swap! (:pmodes st) conj nmask)
        (swap! (:pmut st) conj pm)
        (and (= nmask pmode) (= mc prevmc))))))

(defn solve-case [p a pre add del initmask goalmask]
  (let [n (+ a p)
        st {:p p :a a :n n :pre pre :add add :del del
            :initmask initmask :goalmask goalmask
            :actids (atom []) :amut (atom [])
            :pmodes (atom [initmask])
            :pmut (atom [(vec (repeat p (vec (repeat p false))))])
            :nogood (atom #{})}]
    (loop [l 0]
      (if (> l 12)
        [false nil 0]
        (let [pmode (nth @(:pmodes st) l)
              gs (bits-of p goalmask)
              goals-ok (and (= (bit-and pmode goalmask) goalmask)
                            (every? (fn [[x y]] (not (get-in @(:pmut st) [l x y])))
                                     (for [i (range (count gs)) j (range (inc i) (count gs))]
                                       [(nth gs i) (nth gs j)])))]
          (if goals-ok
            (let [[ok plan] (extract* st l gs)]
              (cond ok [true plan l]
                    (= l 12) [false nil 0]
                    (expand-once st l) [false nil 0]
                    :else (recur (inc l))))
            (if (= l 12)
              [false nil 0]
              (if (expand-once st l) [false nil 0] (recur (inc l))))))))))

(defn parse-case [nums pos]
  (let [p (int (nth nums pos)) a (int (nth nums (+ pos 1)))
        n (+ a p)]
    (loop [i 0 q (+ pos 2) pre (vec (repeat n 0)) add (vec (repeat n 0)) del (vec (repeat n 0))]
      (if (= i a)
        (let [pre2 (reduce (fn [pr x] (assoc pr (+ a x) (bit-shift-left 1 x))) pre (range p))
              add2 (reduce (fn [ad x] (assoc ad (+ a x) (bit-shift-left 1 x))) add (range p))
              ni (int (nth nums q))
              initmask (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums (inc q) (+ q 1 ni))))
              q2 (+ q 1 ni)
              ng (int (nth nums q2))
              goalmask (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums (inc q2) (+ q2 1 ng))))]
          [p a pre2 add2 del initmask goalmask (+ q2 1 ng)])
        (let [np (int (nth nums q))
              q1 (inc q)
              pm (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums q1 (+ q1 np))))
              q2 (+ q1 np)
              na (int (nth nums q2))
              q3 (inc q2)
              am (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums q3 (+ q3 na))))
              q4 (+ q3 na)
              nd (int (nth nums q4))
              q5 (inc q4)
              dm (reduce bit-or 0 (map #(bit-shift-left 1 (int %)) (subvec nums q5 (+ q5 nd))))]
          (recur (inc i) (+ q5 nd)
                 (assoc pre i pm) (assoc add i am) (assoc del i dm)))))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[p a pre add del initmask goalmask p2] (parse-case nums pos)
              [ok plan l] (solve-case p a pre add del initmask goalmask)]
          (if (not ok)
            (.append sb "GRAPHPLAN FAIL\n")
            (do (.append sb (str "GRAPHPLAN " l "\n"))
                (doseq [lvl (range l)]
                  (.append sb (str (str/join " " (sort (filter #(< % a) (nth plan lvl)))) "\n")))))
          (recur p2 (dec tcases) sb))))))

(-main)
