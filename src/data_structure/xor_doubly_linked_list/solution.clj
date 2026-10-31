;; xor-doubly-linked-list: memory-efficient deque (Karumanchi Ch 3).
;; Each node stores prev^next (index+1 encoding, 0=null) in one field.
;; Ops: 1 x push_front, 2 x push_back, 3 pop_front, 4 pop_back, 5 print.

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

(defn alloc [st x]
  (let [n (count (:val st))]
    [(assoc st :val (conj (:val st) x) :link (conj (:link st) 0)) n]))

(defn push-front [st x]
  (let [[st2 n] (alloc st x)
        st3 (assoc st2 :link (assoc (:link st2) n (bit-xor 0 (+ (:head st2) 1))))]
    (if (= (:head st3) -1)
      (assoc st3 :head n :tail n)
      (let [h (:head st3)]
        (assoc st3 :link (assoc (:link st3) h (bit-xor (nth (:link st3) h) (+ n 1))) :head n)))))

(defn push-back [st x]
  (let [[st2 n] (alloc st x)
        st3 (assoc st2 :link (assoc (:link st2) n (bit-xor (+ (:tail st2) 1) 0)))]
    (if (= (:tail st3) -1)
      (assoc st3 :head n :tail n)
      (let [t (:tail st3)]
        (assoc st3 :link (assoc (:link st3) t (bit-xor (nth (:link st3) t) (+ n 1))) :tail n)))))

(defn pop-front [st]
  (if (= (:head st) -1) [st nil]
    (let [o (nth (:val st) (:head st))
          nxt (bit-xor (nth (:link st) (:head st)) 0)]
      (if (= nxt 0) [(assoc st :head -1 :tail -1) o]
        (let [ni (- nxt 1) h (:head st)]
          [(assoc st :link (assoc (:link st) ni (bit-xor (nth (:link st) ni) (+ h 1))) :head ni) o])))))

(defn pop-back [st]
  (if (= (:tail st) -1) [st nil]
    (let [o (nth (:val st) (:tail st))
          prv (bit-xor (nth (:link st) (:tail st)) 0)]
      (if (= prv 0) [(assoc st :head -1 :tail -1) o]
        (let [pi (- prv 1) t (:tail st)]
          [(assoc st :link (assoc (:link st) pi (bit-xor (nth (:link st) pi) (+ t 1))) :tail pi) o])))))

(defn to-vec [st]
  (loop [prev -1 cur (:head st) acc []]
    (if (= cur -1) acc
      (let [nxt (bit-xor (nth (:link st) cur) (+ prev 1))]
        (recur cur (if (= nxt 0) -1 (- nxt 1)) (conj acc (nth (:val st) cur)))))))

(defn solve [toks]
  (let [q (read-int (nth toks 0))]
    (loop [k 0 i 1 st {:val [] :link [] :head -1 :tail -1} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (recur (+ k 1) (+ i 2) (push-front st (read-int (nth toks (+ i 1)))) out)
            (= op 2) (recur (+ k 1) (+ i 2) (push-back st (read-int (nth toks (+ i 1)))) out)
            (= op 3) (let [[st2 o] (pop-front st)]
                       (recur (+ k 1) (+ i 1) st2 (conj out (if (nil? o) "EMPTY" (str o)))))
            (= op 4) (let [[st2 o] (pop-back st)]
                       (recur (+ k 1) (+ i 1) st2 (conj out (if (nil? o) "EMPTY" (str o)))))
            (= op 5) (recur (+ k 1) (+ i 1) st
                            (conj out (apply str (interpose " " (map str (to-vec st))))))
            :else (recur (+ k 1) (+ i 1) st out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
