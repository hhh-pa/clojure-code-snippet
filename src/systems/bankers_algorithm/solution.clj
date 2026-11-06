;; bankers-algorithm: deadlock avoidance safety check (Tanenbaum MOS 6.5).
;; Input: n r; available; n alloc rows; n max rows.
;; Output: SAFE + sequence, or UNSAFE + completed count. Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

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
