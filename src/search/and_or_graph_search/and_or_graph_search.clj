(ns and-or-search (:require [clojure.string :as str]))
(defn solve [typ children memo memo-plan node]
  (if (not= (nth @memo node) -1)
    (if (= (nth @memo node) 0) nil (nth @memo-plan node))
    (cond (= (nth typ node) 2) (do (swap! memo assoc node 1) (swap! memo-plan assoc node #{node}) #{node})
          (= (nth typ node) 3) (do (swap! memo assoc node 0) nil)
          (= (nth typ node) 1) (loop [cs (nth children node) acc #{node}]
                                 (if (empty? cs) (do (swap! memo assoc node 1) (swap! memo-plan assoc node acc) acc)
                                   (if-let [sub (solve typ children memo memo-plan (first cs))]
                                     (recur (rest cs) (into acc sub))
                                     (do (swap! memo assoc node 0) nil))))
          :else (loop [cs (nth children node)]
                  (when (seq cs)
                    (if-let [sub (solve typ children memo memo-plan (first cs))]
                      (let [comb (conj sub node)] (swap! memo assoc node 1) (swap! memo-plan assoc node comb) comb)
                      (recur (rest cs))))))))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+") nums (mapv #(Long/parseLong %) toks) t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases) (print (str sb))
        (let [n (int (nth nums pos)) pos1 (inc pos)
              typs (vec (map int (subvec nums pos1 (+ pos1 n))))
              pos2 (+ pos1 n)
              [children pos3] (loop [i 0 ch (vec (repeat n [])) p pos2]
                                (if (>= i n) [ch p]
                                  (if (or (= (nth typs i) 0) (= (nth typs i) 1))
                                    (let [k (int (nth nums p)) cs (vec (map int (subvec nums (inc p) (+ p 1 k))))]
                                      (recur (inc i) (assoc ch i cs) (+ p 1 k)))
                                    (recur (inc i) ch p))))
              memo (atom (vec (repeat n -1))) memo-plan (atom (vec (repeat n #{})))
              res (solve typs children memo memo-plan 0)]
          (if (nil? res)
            (do (.append sb "UNSOLVABLE\n") (recur pos3 (dec tcases) sb))
            (do (.append sb (str "SOLVABLE " (count res) "\n"))
                (.append sb (str/join " " (sort res))) (.append sb "\n")
                (recur pos3 (dec tcases) sb))))))))
(-main)
