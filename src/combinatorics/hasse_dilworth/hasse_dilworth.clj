;; hasse-dilworth: Hasse diagram via Warshall, height, Dilworth width.
;; Input: n m then edges. Output: cover edges, height, width.
(ns hasse-dilworth (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn warshall [n edges]
  (let [r0 (vec (for [i (range n)] (vec (for [j (range n)] (= i j)))))
        r1 (reduce (fn [r [u v]] (assoc-in r [u v] true)) r0 edges)]
    (loop [k 0 r r1]
      (if (>= k n) r
        (recur (inc k) (vec (for [i (range n)]
                              (vec (for [j (range n)]
                                     (or (nth (nth r i) j)
                                         (and (nth (nth r i) k) (nth (nth r k) j))))))))))))
(defn hasse-edges [n reach]
  (vec (for [u (range n) v (range n)
             :when (and (not= u v) (nth (nth reach u) v)
                        (not (some (fn [w] (and (not= w u) (not= w v)
                                                (nth (nth reach u) w) (nth (nth reach w) v)))
                                   (range n))))]
         [u v])))
(defn topo-order [n hasse]
  (let [adj (loop [i 0 a (vec (repeat n []))]
              (if (>= i (count hasse)) a
                (let [[u v] (nth hasse i)]
                  (recur (inc i) (assoc a u (conj (nth a u) v))))))
        indeg (loop [i 0 d (vec (repeat n 0))]
                (if (>= i (count hasse)) d
                  (recur (inc i) (assoc d (nth (nth hasse i) 1) (inc (nth d (nth (nth hasse i) 1)))))))]
    (loop [avail (sort (filter #(zero? (nth indeg %)) (range n))) indeg indeg topo []]
      (if (empty? avail) topo
        (let [u (first avail)
              [indeg avail2] (loop [vs (sort (nth adj u)) indeg indeg av (vec (rest avail))]
                               (if (empty? vs) [indeg av]
                                 (let [v (first vs) nd (dec (nth indeg v)) indeg2 (assoc indeg v nd)]
                                   (recur (rest vs) indeg2 (if (zero? nd) (vec (sort (conj av v))) av)))))]
          (recur avail2 indeg (conj topo u)))))))
(defn kuhn [u reach matchR seen]
  (loop [v 0 matchR matchR seen seen]
    (if (>= v (count reach)) [false matchR seen]
      (if (or (= u v) (not (nth (nth reach u) v)) (nth seen v))
        (recur (inc v) matchR seen)
        (let [seen2 (assoc seen v true)]
          (if (neg? (nth matchR v))
            [true (assoc matchR v u) seen2]
            (let [[ok m2 s3] (kuhn (nth matchR v) reach matchR seen2)]
              (if ok [true (assoc m2 v u) s3]
                (recur (inc v) matchR seen2)))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            n (nth vals 0) m (nth vals 1)
            edges (vec (for [i (range m)] [(nth vals (+ 2 (* 2 i))) (nth vals (+ 3 (* 2 i)))]))
            reach (warshall n edges)
            hasse (hasse-edges n reach)
            topo (topo-order n hasse)
            h (loop [i 0 h (vec (repeat n 1))]
                (if (>= i (count topo)) h
                  (let [v (nth topo i)]
                    (recur (inc i) (loop [u 0 h h]
                                     (if (>= u n) h
                                       (recur (inc u) (if (and (not= u v) (nth (nth reach u) v))
                                                        (assoc h v (max (nth h v) (inc (nth h u)))) h))))))))
            matchR (loop [u 0 matchR (vec (repeat n -1))]
                     (if (>= u n) matchR
                       (let [[ok m2 _] (kuhn u reach matchR (vec (repeat n false)))]
                         (recur (inc u) m2))))
            matching (count (filter #(>= % 0) matchR))]
        (println (count hasse))
        (doseq [[u v] hasse] (println u v))
        (println (apply max h))
        (println (- n matching))))))
(-main)
