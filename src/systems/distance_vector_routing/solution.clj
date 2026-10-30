;; distance-vector-routing: Bellman-Ford + split horizon/poison reverse.
;; Input: N E; E undirected links u v cost. Synchronous DV to
;; convergence. Output: rounds; per node i: dist+nexthop to node N-1
;; ("0 -" for self). Pure Clojure.

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

(def INFLL 4000000000000000000)

(defn dv-round [N cost nbr dist nx]
  (let [adv (mapv (fn [i] (mapv (fn [nb] (mapv (fn [d] (if (and (= (nth (nth nx i) d) nb) (not= d i)) INFLL (nth (nth dist i) d))) (range N))) (range N)))
                  (range N))]
    (loop [i 0 nd dist nn nx ch false]
      (if (>= i N) [nd nn ch]
        (let [[nd2 nn2 ch2] (loop [d 0 a nd b nn c ch]
                              (if (>= d N) [a b c]
                                (if (= d i) (recur (+ d 1) a b c)
                                  (let [[best bn] (loop [nbs (nth nbr i) bst INFLL bbn -1]
                                                    (if (empty? nbs) [bst bbn]
                                                      (let [nb (first nbs)
                                                            av (nth (nth (nth adv nb) i) d)
                                                            cand (if (>= av (quot INFLL 2)) INFLL (+ (nth (nth cost i) nb) av))]
                                                        (recur (rest nbs)
                                                               (if (or (< cand bst) (and (= cand bst) (or (= bbn -1) (< nb bbn)))) cand bst)
                                                               (if (or (< cand bst) (and (= cand bst) (or (= bbn -1) (< nb bbn)))) nb bbn)))))]
                                    (if (or (not= best (nth (nth dist i) d)) (not= bn (nth (nth nx i) d)))
                                      (recur (+ d 1) (assoc a i (assoc (nth a i) d best)) (assoc b i (assoc (nth b i) d bn)) true)
                                      (recur (+ d 1) a b c))))))]
          (recur (+ i 1) nd2 nn2 ch2))))))

(defn solve [toks]
  (let [N (read-int (nth toks 0)) E (read-int (nth toks 1))
        [cost nbr] (loop [i 0 c (vec (repeat N (vec (repeat N INFLL)))) nb (vec (repeat N []))]
                     (if (>= i E) [c nb]
                       (let [u (read-int (nth toks (+ 2 (* i 3))))
                             v (read-int (nth toks (+ 3 (* i 3))))
                             cc (read-int (nth toks (+ 4 (* i 3))))]
                         (recur (+ i 1)
                                (assoc c u (assoc (nth c u) v cc) v (assoc (nth c v) u cc))
                                (assoc nb u (conj (nth nb u) v) v (conj (nth nb v) u))))))
        dist0 (mapv (fn [i] (assoc (reduce (fn [row nb] (assoc row nb (nth (nth cost i) nb))) (vec (repeat N INFLL)) (nth nbr i)) i 0)) (range N))
        nx0 (mapv (fn [i] (assoc (reduce (fn [row nb] (assoc row nb nb)) (vec (repeat N -1)) (nth nbr i)) i i)) (range N))
        [dist nx rounds] (loop [r 0 dist dist0 nx nx0]
                           (if (>= r 1000) [dist nx r]
                             (let [[nd nn ch] (dv-round N cost nbr dist nx)]
                               (if ch (recur (+ r 1) nd nn) [nd nn r]))))
        dst (- N 1)]
    (into [(str rounds)]
          (mapv (fn [i] (cond (= i dst) "0 -"
                             (>= (nth (nth dist i) dst) (quot INFLL 2)) "INF -"
                             :else (str (nth (nth dist i) dst) " " (nth (nth nx i) dst)))) (range N)))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
