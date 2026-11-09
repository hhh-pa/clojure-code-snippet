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
  (when (and (string? tok) (pos? (count tok)))
    (parse-long tok)))

(def P 1000003)
(def WRAP_MIN (- P 500))

(defn bucket-of [a b m k] (mod (mod (+ (* a k) b) P) m))

(defn cand-bs [xs n]
  (loop [i 0 acc [0]]
    (if (= i n)
      (if (> (count acc) 1) (vec (distinct (sort acc))) acc)
      (let [x (nth xs i)]
        (if (>= x WRAP_MIN)
          (recur (inc i) (conj acc (- P x)))
          (recur (inc i) acc))))))

(defn level1 [keys m]
  (let [n (count keys)
        km (mapv #(mod % P) keys)
        zeros (vec (repeat m 0))]
    (if (<= n 1)
      [1 0 n]
      (loop [a 1 best-a 1 best-b 0 best-s -1]
        (if (or (> a 500) (= best-s n))
          [best-a best-b best-s]
          (let [xs (loop [i 0 acc (transient [])]
                     (if (= i n)
                       (persistent! acc)
                       (recur (inc i) (conj! acc (rem (* a (nth km i)) P)))))
                bs-list (cand-bs xs n)
                nb (count bs-list)
                [ba bb bs]
                (loop [bi 0 ba best-a bb best-b bs best-s]
                  (if (or (= bi nb) (= bs n))
                    [ba bb bs]
                    (let [b (nth bs-list bi)
                          cnt (transient zeros)
                          s (loop [i 0 s 0]
                              (if (= i n)
                                s
                                (let [x (nth xs i)
                                      v (+ x b)
                                      vp (if (>= v P) (- v P) v)
                                      idx (rem vp m)
                                      c (nth cnt idx)
                                      ns (+ s (* 2 c) 1)]
                                  (if (and (not= bs -1) (>= ns bs))
                                    ns
                                    (do (assoc! cnt idx (inc c))
                                        (recur (inc i) ns))))))]
                      (if (or (= bs -1) (< s bs))
                        (recur (inc bi) a b s)
                        (recur (inc bi) ba bb bs)))))]
            (recur (inc a) ba bb bs)))))))

(defn collisions [xs nb b mj limit]
  (loop [i 0 seen #{} col 0]
    (if (or (= i nb) (>= col limit))
      col
      (let [x (nth xs i)
            v (+ x b)
            vp (if (>= v P) (- v P) v)
            s (rem vp mj)]
        (if (contains? seen s)
          (recur (inc i) seen (inc col))
          (recur (inc i) (conj seen s) col))))))

(defn level2 [bucket]
  (let [nj (count bucket)
        mj (if (> nj 1) (* nj nj) 1)]
    (if (<= nj 1)
      [mj 1 0]
      (let [km (mapv #(mod % P) bucket)]
        (loop [a 1 ba 1 bb 0 bc (inc nj)]
          (if (or (> a 500) (zero? bc))
            [mj ba bb]
            (let [xs (loop [i 0 acc (transient [])]
                       (if (= i nj)
                         (persistent! acc)
                         (recur (inc i) (conj! acc (rem (* a (nth km i)) P)))))
                  bs-list (cand-bs xs nj)
                  nb (count bs-list)
                  [ba2 bb2 bc2]
                  (loop [bi 0 ba ba bb bb bc bc]
                    (if (or (= bi nb) (zero? bc))
                      [ba bb bc]
                      (let [b (nth bs-list bi)
                            c (collisions xs nj b mj bc)]
                        (if (< c bc)
                          (recur (inc bi) a b c)
                          (recur (inc bi) ba bb bc)))))]
              (recur (inc a) ba2 bb2 bc2))))))))

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
