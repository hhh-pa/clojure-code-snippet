;; tarjan-lca: offline lowest common ancestors (CLRS Ch 19 Prob).
;; DFS + DSU with ancestor tracking. Input: n; n-1 edges; q; q queries.
;; Output LCA per query. Nodes 1-based, root 1. Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
                           {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(declare tarjan-dfs)

(defn dsu-find [p x]
  (loop [p p x x trail []]
    (if (= (nth p x) x)
      [(reduce (fn [pp t] (assoc pp t x)) p trail) x]
      (recur p (nth p x) (conj trail x)))))

(defn answer-queries [st u]
  (loop [st st qs (nth (:qadj st) u)]
    (if (empty? qs) st
      (let [[v qi] (first qs) rest-qs (rest qs)]
        (if (nth (:visited st) v)
          (let [[p1 f] (dsu-find (:parent st) v)
                a (nth (:ancestor st) f)]
            (recur (assoc st :parent p1 :ans (assoc (:ans st) qi a)) rest-qs))
          (recur st rest-qs))))))

(defn tarjan-dfs [st u p]
  (let [st0 (assoc st :parent (assoc (:parent st) u u) :ancestor (assoc (:ancestor st) u u))
        st1 (loop [st st0 nbrs (nth (:adj st0) u)]
              (if (empty? nbrs) st
                (let [v (first nbrs) more (rest nbrs)]
                  (if (= v p) (recur st more)
                    (let [st2 (tarjan-dfs st v u)
                          [p3 fu] (dsu-find (:parent st2) u)
                          [p4 fv] (dsu-find p3 v)
                          p5 (assoc p4 fv fu)
                          [p6 f2] (dsu-find p5 u)]
                      (recur (assoc st2 :parent p6 :ancestor (assoc (:ancestor st2) f2 u)) more))))))
        st2 (assoc st1 :visited (assoc (:visited st1) u true))]
    (answer-queries st2 u)))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        adj (loop [i 0 a (vec (repeat (+ n 1) []))]
              (if (>= i (- n 1)) a
                (let [u (read-int (nth toks (+ 1 (* i 2)))) v (read-int (nth toks (+ 2 (* i 2))))]
                  (recur (+ i 1) (assoc a u (conj (nth a u) v) v (conj (nth a v) u))))))
        base (+ 1 (* (- n 1) 2))
        q (read-int (nth toks base))
        qadj (loop [i 0 qa (vec (repeat (+ n 1) []))]
               (if (>= i q) qa
                 (let [a (read-int (nth toks (+ base 1 (* i 2)))) b (read-int (nth toks (+ base 2 (* i 2))))]
                   (recur (+ i 1) (assoc qa a (conj (nth qa a) [b i]) b (conj (nth qa b) [a i]))))))
        st {:adj adj :qadj qadj :parent (vec (range (+ n 1))) :ancestor (vec (repeat (+ n 1) 0))
            :visited (vec (repeat (+ n 1) false)) :ans (vec (repeat q 0))}
        done (tarjan-dfs st 1 0)]
    (mapv str (:ans done))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
