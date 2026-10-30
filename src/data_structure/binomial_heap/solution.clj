;; binomial-heap: mergeable min-heap via binomial trees (CLRS Ch 19).
;; Line1: q. Ops: 1 x insert, 2 extract-min, 3 peek. Pure Clojure.

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

(defn bh-link [h y z]
  (assoc h :parent (assoc (:parent h) y z)
         :sibling (assoc (:sibling h) y (nth (:child h) z))
         :child (assoc (:child h) z y)
         :degree (assoc (:degree h) z (+ (nth (:degree h) z) 1))))

(defn bh-merge [h h1 h2]
  (cond (= h1 -1) [h h2]
        (= h2 -1) [h h1]
        :else (let [[hd t r1 r2] (if (<= (nth (:degree h) h1) (nth (:degree h) h2))
                                   [h1 h1 (nth (:sibling h) h1) h2]
                                   [h2 h2 h1 (nth (:sibling h) h2)])]
                (loop [h h t t r1 r1 r2 r2]
                  (cond (and (not= r1 -1) (not= r2 -1))
                        (if (<= (nth (:degree h) r1) (nth (:degree h) r2))
                          (recur (assoc h :sibling (assoc (:sibling h) t r1)) r1 (nth (:sibling h) r1) r2)
                          (recur (assoc h :sibling (assoc (:sibling h) t r2)) r2 r1 (nth (:sibling h) r2)))
                        :else [(assoc h :sibling (assoc (:sibling h) t (if (not= r1 -1) r1 r2))) hd])))))

(defn bh-unite [h h1 h2]
  (let [[h hd] (bh-merge h h1 h2)]
    (if (= hd -1) [h -1]
      (loop [h h hd hd prev -1 x hd nxt (nth (:sibling h) hd)]
        (if (= nxt -1) [h hd]
          (let [sn (nth (:sibling h) nxt)]
            (cond (or (not= (nth (:degree h) x) (nth (:degree h) nxt))
                      (and (not= sn -1) (= (nth (:degree h) sn) (nth (:degree h) x))))
                  (recur h hd x nxt (nth (:sibling h) nxt))
                  (<= (nth (:key h) x) (nth (:key h) nxt))
                  (let [h2 (assoc h :sibling (assoc (:sibling h) x (nth (:sibling h) nxt)))
                        h3 (bh-link h2 nxt x)]
                    (recur h3 hd prev x (nth (:sibling h3) x)))
                  :else (let [hd2 (if (= prev -1) nxt hd)
                              h2 (if (= prev -1) h (assoc h :sibling (assoc (:sibling h) prev nxt)))
                              h3 (bh-link h2 x nxt)]
                          (recur h3 hd2 prev nxt (nth (:sibling h3) nxt))))))))))

(defn bh-insert [h x]
  (let [n (count (:key h))
        h2 (assoc h :key (conj (:key h) x) :parent (conj (:parent h) -1)
                 :child (conj (:child h) -1) :sibling (conj (:sibling h) -1)
                 :degree (conj (:degree h) 0))
        [h3 hd] (bh-unite h2 (:head h2) n)]
    (assoc h3 :head hd)))

(defn bh-minimum [h]
  (if (= (:head h) -1) nil
    (loop [r (nth (:sibling h) (:head h)) o (nth (:key h) (:head h))]
      (if (= r -1) o
        (recur (nth (:sibling h) r) (if (< (nth (:key h) r) o) (nth (:key h) r) o))))))

(defn bh-extract [h]
  (if (= (:head h) -1) [h nil]
    (let [[m mp] (loop [r (:head h) m (:head h) mp -1 p -1]
                   (if (= r -1) [m mp]
                     (if (< (nth (:key h) r) (nth (:key h) m))
                       (recur (nth (:sibling h) r) r p r)
                       (recur (nth (:sibling h) r) m mp r))))
          o (nth (:key h) m)
          h2 (if (= mp -1) (assoc h :head (nth (:sibling h) m))
               (assoc h :sibling (assoc (:sibling h) mp (nth (:sibling h) m))))
          [h3 rev] (loop [h h2 c (nth (:child h2) m) rev -1]
                     (if (= c -1) [h rev]
                       (let [nx (nth (:sibling h) c)]
                         (recur (assoc h :sibling (assoc (:sibling h) c rev)
                                       :parent (assoc (:parent h) c -1)) nx c))))
          [h4 hd] (bh-unite h3 (:head h3) rev)]
      [(assoc h4 :head hd) o])))

(defn solve [toks]
  (let [q (read-int (nth toks 0))]
    (loop [k 0 i 1 h {:key [] :parent [] :child [] :sibling [] :degree [] :head -1} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (recur (+ k 1) (+ i 2) (bh-insert h (read-int (nth toks (+ i 1)))) out)
            (= op 2) (let [[h2 o] (bh-extract h)]
                       (recur (+ k 1) (+ i 1) h2 (conj out (if (nil? o) "EMPTY" (str o)))))
            (= op 3) (let [o (bh-minimum h)]
                       (recur (+ k 1) (+ i 1) h (conj out (if (nil? o) "EMPTY" (str o)))))
            :else (recur (+ k 1) (+ i 1) h out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
