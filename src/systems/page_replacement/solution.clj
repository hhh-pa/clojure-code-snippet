;; page-replacement: OPT, FIFO, Clock, LRU fault counts (Tanenbaum MOS 3.4).
;; Input: frames nrefs; reference string. Output one line:
;; "<opt> <fifo> <clock> <lru>". Pure Clojure.

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

(defn sim-opt [refs f n]
  (loop [i 0 frames [] faults 0]
    (if (>= i n) faults
      (if (some (fn [p] (= p (nth refs i))) frames)
        (recur (+ i 1) frames faults)
        (if (< (count frames) f)
          (recur (+ i 1) (conj frames (nth refs i)) (+ faults 1))
          (let [victim (loop [s 0 best 0 far -1]
                         (if (>= s (count frames)) best
                           (let [nx (loop [j (+ i 1)] (cond (>= j n) (+ n 1)
                                                            (= (nth refs j) (nth frames s)) j
                                                            :else (recur (+ j 1))))]
                             (recur (+ s 1) (if (> nx far) s best) (max nx far)))))]
            (recur (+ i 1) (assoc frames victim (nth refs i)) (+ faults 1))))))))

(defn sim-fifo [refs f n]
  (loop [i 0 frames [] head 0 faults 0]
    (if (>= i n) faults
      (if (some (fn [p] (= p (nth refs i))) frames)
        (recur (+ i 1) frames head faults)
        (if (< (count frames) f)
          (recur (+ i 1) (conj frames (nth refs i)) head (+ faults 1))
          (recur (+ i 1) (assoc frames head (nth refs i)) (mod (+ head 1) (count frames)) (+ faults 1)))))))

(defn sim-clock [refs f n]
  (loop [i 0 frames (vec (repeat f 0)) occ (vec (repeat f false)) rf (vec (repeat f false)) hand 0 faults 0]
    (if (>= i n) faults
      (let [hit (loop [s 0] (cond (>= s f) -1 (and (nth occ s) (= (nth frames s) (nth refs i))) s :else (recur (+ s 1))))]
        (if (>= hit 0)
          (recur (+ i 1) frames occ (assoc rf hit true) hand faults)
          (let [[hand2 rf2] (loop [hd hand rf2 rf]
                                  (cond (not (nth occ hd)) [hd rf2]
                                        (nth rf2 hd) (recur (mod (+ hd 1) f) (assoc rf2 hd false))
                                        :else [hd rf2]))]
            (recur (+ i 1) (assoc frames hand2 (nth refs i)) (assoc occ hand2 true)
                   (assoc rf2 hand2 true) (mod (+ hand2 1) f) (+ faults 1))))))))

(defn sim-lru [refs f n]
  (loop [i 0 frames [] faults 0]
    (if (>= i n) faults
      (let [at (loop [s 0] (cond (>= s (count frames)) -1 (= (nth frames s) (nth refs i)) s :else (recur (+ s 1))))]
        (if (>= at 0)
          (recur (+ i 1) (conj (vec (concat (subvec frames 0 at) (subvec frames (+ at 1)))) (nth refs i)) faults)
          (if (< (count frames) f)
            (recur (+ i 1) (conj frames (nth refs i)) (+ faults 1))
            (recur (+ i 1) (conj (subvec frames 1) (nth refs i)) (+ faults 1))))))))

(defn solve [toks]
  (let [f (read-int (nth toks 0)) n (read-int (nth toks 1))
        refs (mapv (fn [i] (read-int (nth toks (+ 2 i)))) (range n))]
    (str (sim-opt refs f n) " " (sim-fifo refs f n) " " (sim-clock refs f n) " " (sim-lru refs f n))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
