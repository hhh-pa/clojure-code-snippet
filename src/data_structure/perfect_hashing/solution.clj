;; perfect-hashing: FKS two-level static table (CLRS 11.5).
;; Input: n; n distinct keys; q; q queries. Deterministic grid search
;; for level-1 and collision-free level-2 parameters. Output: total
;; slots S, then YES/NO per query. Pure Clojure.
(require '[clojure.string :as str])

(defn tokenize
  [s]
  (if (nil? s)
    []
    (let [v (str/split s #"[ \t\n\r\f\x0B]+")
          n (count v)
          start (if (and (pos? n) (str/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (str/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn read-int
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(def P 1000003)

(defn bucket-of [a b m k] (mod (mod (+ (* a k) b) P) m))

(defn level1 [keys m]
  (let [n (count keys)
        zeros (vec (repeat m 0))]
    (if (<= n 1)
      [1 0 n]
      (loop [a 1 best-a 1 best-b 0 best-s -1]
        (if (or (> a 500) (= best-s n))
          [best-a best-b best-s]
          (let [[ba bb bs]
                (loop [b 0 ba best-a bb best-b bs best-s]
                  (if (or (> b 500) (= bs n))
                    [ba bb bs]
                    (let [cnt (transient zeros)
                          s (loop [i 0 s 0]
                              (if (= i n)
                                s
                                (let [k (nth keys i)
                                      idx (bucket-of a b m k)
                                      c (nth cnt idx)
                                      ns (+ s (* 2 c) 1)]
                                  (if (and (not= bs -1) (>= ns bs))
                                    ns
                                    (do (assoc! cnt idx (inc c))
                                        (recur (inc i) ns))))))]
                      (if (or (= bs -1) (< s bs))
                        (recur (inc b) a b s)
                        (recur (inc b) ba bb bs)))))]
            (recur (inc a) ba bb bs)))))))

(defn collisions [bucket a b mj limit]
  (let [nb (count bucket)]
    (loop [i 0 seen #{} col 0]
      (if (or (= i nb) (>= col limit))
        col
        (let [k (nth bucket i)
              s (mod (mod (+ (* a k) b) P) mj)]
          (if (contains? seen s)
            (recur (inc i) seen (inc col))
            (recur (inc i) (conj seen s) col)))))))

(defn level2 [bucket]
  (let [nj (count bucket)
        mj (if (> nj 1) (* nj nj) 1)]
    (if (<= nj 1)
      [mj 1 0]
      (loop [a 1 ba 1 bb 0 bc (inc nj)]
        (if (or (> a 500) (zero? bc))
          [mj ba bb]
          (let [[ba2 bb2 bc2]
                (loop [b 0 ba ba bb bb bc bc]
                  (if (or (> b 500) (zero? bc))
                    [ba bb bc]
                    (let [c (collisions bucket a b mj bc)]
                      (if (< c bc)
                        (recur (inc b) a b c)
                        (recur (inc b) ba bb bc)))))]
            (recur (inc a) ba2 bb2 bc2)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        keys (loop [i 0 acc (transient [])]
               (if (= i n)
                 (persistent! acc)
                 (recur (inc i) (conj! acc (read-int (nth toks (inc i)))))))
        q (read-int (nth toks (inc n)))
        m (if (pos? n) n 1)
        [a1 b1 _] (level1 keys m)
        buckets (reduce (fn [bk k]
                          (let [h (bucket-of a1 b1 m k)]
                            (assoc bk h (conj (nth bk h) k))))
                        (vec (repeat m [])) keys)
        l2 (mapv level2 buckets)
        S (reduce + 0 (mapv first l2))
        slots (mapv (fn [j]
                      (let [[mj ba bb] (nth l2 j)]
                        (reduce (fn [sl k]
                                  (let [h (mod (mod (+ (* ba k) bb) P) mj)]
                                    (assoc sl h (conj (nth sl h) k))))
                                (vec (repeat mj [])) (nth buckets j))))
                    (range m))
        base (+ 2 n)]
    (loop [i 0 out (transient [(str S)])]
      (if (= i q)
        (persistent! out)
        (let [x (read-int (nth toks (+ base i)))
              j (bucket-of a1 b1 m x)
              [mj ba bb] (nth l2 j)
              s (mod (mod (+ (* ba x) bb) P) mj)
              cell (nth (nth slots j) s)
              hit? (loop [idx 0]
                     (cond
                       (= idx (count cell)) false
                       (= (nth cell idx) x) true
                       :else (recur (inc idx))))]
          (recur (inc i) (conj! out (if hit? "YES" "NO"))))))))

(let [toks (tokenize (slurp *in*))]
  (when (pos? (count toks))
    (print (str (str/join "\n" (solve toks)) "\n"))))
