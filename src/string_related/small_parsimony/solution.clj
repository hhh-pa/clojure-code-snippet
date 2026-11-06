;; small-parsimony: minimum-mutation ancestral state assignment.
;; Fitch's bottom-up set DP counts union-nodes; Sankoff's cost DP
;; (unit costs) minimizes over the observed alphabet. Output: Fitch
;; score, Sankoff score, then the sorted Fitch root set.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))
(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))
(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))
(def INF 1000000000)
(defn -main []
  (let [toks (tokenize (slurp *in*)) pos (atom 0)
        next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
        n (parse-token (next-tok))
        ch (loop [e 0 ch (vec (repeat n []))]
             (if (>= e (dec n)) ch
               (let [p (parse-token (next-tok)) c (parse-token (next-tok))]
                 (recur (inc e) (assoc ch p (conj (nth ch p) c))))))
        L (parse-token (next-tok))
        [leaf alpha] (loop [i 0 leaf (vec (repeat n nil)) alpha []]
                       (if (>= i L) [leaf alpha]
                         (let [id (parse-token (next-tok)) s (next-tok)]
                           (recur (inc i) (assoc leaf id (nth s 0)) (conj alpha (nth s 0))))))
        alpha (vec (sort (set alpha)))
        ai (zipmap alpha (range))
        order (loop [st [0] seen #{0} order []]
                (if (empty? st) order
                  (let [u (peek st)]
                    (recur (into (pop st) (remove seen (nth ch u)))
                           (into seen (nth ch u)) (conj order u)))))
        [fs fcost] (loop [ks (reverse order) fs (vec (repeat n #{})) cost 0]
                     (if (empty? ks) [fs cost]
                       (let [u (first ks) vs (nth ch u)]
                         (if (empty? vs)
                           (recur (rest ks) (assoc fs u #{(nth leaf u)}) cost)
                           (let [mm (mapv (fn [a] (count (remove (fn [v] (contains? (nth fs v) a)) vs))) alpha)
                                 best (apply min mm)
                                 bs (set (map first (filter (fn [[a m]] (= m best)) (map vector alpha mm))))]
                             (recur (rest ks) (assoc fs u bs) (+ cost best)))))))
        sc (loop [ks (reverse order) sc (vec (repeat n (vec (repeat (count alpha) INF))))]
             (if (empty? ks) sc
               (let [u (first ks) vs (nth ch u)]
                 (if (empty? vs)
                   (recur (rest ks) (assoc-in sc [u (get ai (nth leaf u))] 0))
                   (recur (rest ks) (assoc sc u (mapv (fn [a]
                                                        (reduce + (map (fn [v]
                                                                         (apply min (map (fn [b] (+ (nth (nth sc v) b) (if (= a b) 0 1))) (range (count alpha)))))
                                                                       vs)))
                                                      (range (count alpha)))))))))
        scost (apply min (nth sc 0))]
    (println fcost scost)
    (println (apply str (sort (nth fs 0))))))
(-main)
