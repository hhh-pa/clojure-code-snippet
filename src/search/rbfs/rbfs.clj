
;; rbfs: Recursive Best-First Search on an undirected weighted graph with a
;; given heuristic table. Linear-space best-first with backed-up f-limits;
;; successors sorted by (f, node), parent f inherited via max.
(ns rbfs
  (:require [clojure.string :as str]))

(def ^:private INF 1000000000)

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(declare rbfs)

(defn rbfs [adj h goal v g f-limit onpath path]
  (if (= v goal)
    [-1 (conj path v)]
    (let [succs (vec (for [[to w] (nth adj v) :when (not (contains? onpath to))]
                       [(+ w g (nth h to)) to w]))]
      (if (empty? succs)
        [INF path]
        (let [fv (+ g (nth h v))
              s0 (mapv (fn [[f to w]] [(max f fv) to w]) succs)]
          (loop [ss s0]
            (let [srt (sort-by (juxt first second) ss)
                  best (first (first srt))]
              (if (or (>= best INF) (> best f-limit))
                [best path]
                (let [alt (if (> (count srt) 1) (first (second srt)) INF)
                      [_ to w] (first srt)
                      [r p2] (rbfs adj h goal to (+ g w) (min f-limit alt)
                                  (conj onpath to) (conj path v))]
                  (if (< r 0)
                    [-1 p2]
                    (recur (assoc (vec srt) 0 [r to w]))))))))))))

(defn solve [adj h s g]
  (if (= s g)
    [0 [s]]
    (let [[r p] (rbfs adj h g s 0 INF #{s} [])]
      (if (< r 0)
        (let [d (loop [i 1 acc 0]
                  (if (>= i (count p))
                    acc
                    (let [a (nth p (dec i)) b (nth p i)
                          w (some (fn [[to w]] (when (= to b) w)) (nth adj a))]
                      (recur (inc i) (+ acc w)))))]
          [d p])
        [-1 nil]))))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (int (nth nums pos)) m (int (nth nums (+ pos 1)))
              s (int (nth nums (+ pos 2))) g (int (nth nums (+ pos 3)))
              p0 (+ pos 4)
              adj0 (vec (repeat n []))
              adj1 (reduce (fn [a i]
                             (let [u (int (nth nums (+ p0 (* i 3))))
                                   v (int (nth nums (+ p0 (* i 3) 1)))
                                   w (int (nth nums (+ p0 (* i 3) 2)))]
                               (-> a
                                   (update u conj [v w])
                                   (update v conj [u w]))))
                           adj0 (range m))
              adj (mapv #(sort-by (juxt first second) %) adj1)
              p1 (+ p0 (* m 3))
              h (vec (for [i (range n)] (int (nth nums (+ p1 i)))))
              [dist fp] (solve adj h s g)]
          (if (< dist 0)
            (.append sb "RBFS -1\n")
            (.append sb (str "RBFS " dist " " (str/join " " fp) "\n")))
          (recur (+ p1 n) (dec tcases) sb))))))

(-main)
