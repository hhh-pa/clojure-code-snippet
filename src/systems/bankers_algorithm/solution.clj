;; bankers-algorithm: deadlock avoidance safety check (Tanenbaum MOS 6.5).
;; Input: n r; available; n alloc rows; n max rows.
;; Output: SAFE + sequence, or UNSAFE + completed count. Pure Clojure.

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

(defn solve [toks]
  (let [n (read-int (nth toks 0)) r (read-int (nth toks 1))
        nums (mapv read-int (subvec toks 2))
        avail (subvec nums 0 r)
        alloc (mapv (fn [i] (subvec nums (+ r (* i r)) (+ r (* (+ i 1) r)))) (range n))
        mx (mapv (fn [i] (subvec nums (+ r (* n r) (* i r)) (+ r (* n r) (* (+ i 1) r)))) (range n))]
    (loop [work avail done (vec (repeat n false)) seq []]
      (let [pick (loop [i 0]
                   (cond (>= i n) -1
                         (nth done i) (recur (+ i 1))
                         (every? (fn [j] (<= (- (nth (nth mx i) j) (nth (nth alloc i) j)) (nth work j))) (range r)) i
                         :else (recur (+ i 1))))]
        (if (= pick -1)
          (if (= (count seq) n)
            (str "SAFE" (apply str (map (fn [i] (str " " i)) seq)))
            (str "UNSAFE " (count seq)))
          (recur (mapv + work (nth alloc pick)) (assoc done pick true) (conj seq pick)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
