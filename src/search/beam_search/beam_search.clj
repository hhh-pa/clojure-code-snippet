(ns beam-search (:require [clojure.string :as str]))
(defn attacking [b] (let [n (count b)] (count (for [i (range n) j (range (inc i) n) :when (let [bi (nth b i) bj (nth b j)] (or (= bi bj) (= (Math/abs (- bi bj)) (Math/abs (- i j)))))] 1))))
(defn lcg-next [s] (unchecked-add (unchecked-multiply s 6364136223846793005) 1442695040888963407))
(defn beam-search [n k seed]
  (let [gen-boards (loop [s seed acc []]
                     (if (= (count acc) k) acc
                       (let [[boards s2] (loop [b [] s s]
                                           (if (= (count b) n) [b s]
                                             (let [ns (lcg-next s)] (recur (conj b (int (Long/remainderUnsigned ns n))) ns))))]
                         (recur s2 (conj acc boards)))))
        best-init (apply min-key attacking gen-boards)]
    (loop [beam gen-boards best best-init bestc (attacking best-init) iter 0]
      (if (or (zero? bestc) (>= iter 100)) [best bestc]
        (let [succs (sort-by (juxt first second)
                             (for [b beam r (range n) c (range n) :when (not= c (nth b r))
                                   :let [nb (assoc b r c)]] [(attacking nb) nb]))
              new-beam (loop [ss succs acc []]
                         (if (or (empty? ss) (>= (count acc) k)) acc
                           (let [[_ nb] (first ss)] (if (some #(= % nb) acc) (recur (rest ss) acc) (recur (rest ss) (conj acc nb))))))
              new-best (reduce (fn [[bb bc] b] (let [c (attacking b)] (if (< c bc) [b c] [bb bc]))) [best bestc] new-beam)]
          (recur new-beam (first new-best) (second new-best) (inc iter)))))))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+") nums (mapv #(Long/parseLong %) toks) t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases) (print (str sb))
        (let [n (nth nums pos) k (nth nums (inc pos)) seed (nth nums (+ pos 2))
              [best bestc] (beam-search (int n) (int k) seed)]
          (.append sb (str/join " " best)) (.append sb "\n") (.append sb (str bestc)) (.append sb "\n")
          (recur (+ pos 3) (dec tcases) sb))))))
(-main)
