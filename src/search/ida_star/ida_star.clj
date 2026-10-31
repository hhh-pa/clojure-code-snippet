
;; ida_star: Iterative Deepening A* on an undirected weighted graph with a
;; given heuristic table. Depth-first f-limited search, bound raised to the
;; minimum pruned f each round; ascending neighbors, on-path cycle check.
(ns ida-star
  (:require [clojure.string :as str]))

(def ^:private INF 1000000000)

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(declare dfs)

(defn dfs [adj h goal v g bound onpath path]
  (let [f (+ g (nth h v))]
    (if (> f bound)
      [f path]
      (if (= v goal)
        [-1 (conj path v)]
        (loop [nbrs (nth adj v) best INF found nil]
          (if (empty? nbrs)
            [best (if found found path)]
            (let [[to w] (first nbrs)]
              (if (contains? onpath to)
                (recur (rest nbrs) best found)
                (let [[r p2] (dfs adj h goal to (+ g w) bound
                                  (conj onpath to) (conj path v))]
                  (if (< r 0)
                    [-1 p2]
                    (recur (rest nbrs) (min best r) found)))))))))))

(defn ida [adj h s g]
  (if (= s g)
    [0 [s]]
    (loop [bound (nth h s) iter 0]
      (if (>= iter 100000)
        [-1 nil]
        (let [[r p] (dfs adj h g s 0 bound #{s} [])]
          (cond (< r 0)
                (let [d (loop [i 1 acc 0]
                          (if (>= i (count p))
                            acc
                            (let [a (nth p (dec i)) b (nth p i)
                                  w (some (fn [[to w]] (when (= to b) w)) (nth adj a))]
                              (recur (inc i) (+ acc w)))))]
                  [d p])
                (>= r INF) [-1 nil]
                :else (recur r (inc iter))))))))

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
              [dist fp] (ida adj h s g)]
          (if (< dist 0)
            (.append sb "IDA -1\n")
            (.append sb (str "IDA " dist " " (str/join " " fp) "\n")))
          (recur (+ p1 n) (dec tcases) sb))))))

(-main)
