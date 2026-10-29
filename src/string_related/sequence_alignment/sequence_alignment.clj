;; Sequence alignment: global Needleman-Wunsch + local Smith-Waterman. Port of
;; cpp/src/string_related/sequence_alignment/sequence_alignment.cc.
;; Input: mode token then two sequence tokens. Mode GLOBAL selects global
;; alignment, anything else selects local. Output: score, then the two
;; aligned strings.

(def MATCH 2)
(def MISMATCH -1)
(def GAP -2)

(defn- sub-score [c1 c2]
  (if (= c1 c2) MATCH MISMATCH))

(defn- global-table [s1 s2]
  (let [n (count s1)
        m (count s2)
        row0 (mapv #(* GAP %) (range (inc m)))]
    (reduce
     (fn [rows i]
       (let [prev (peek rows)
             c1 (nth s1 (dec i))
             row (reduce
                  (fn [r j]
                    (let [diag (+ (nth prev (dec j))
                                  (sub-score c1 (nth s2 (dec j))))
                          up (+ (nth prev j) GAP)
                          left (+ (peek r) GAP)]
                      (conj r (max diag up left))))
                  [(+ (nth prev 0) GAP)]
                  (range 1 (inc m)))]
         (conj rows row)))
     [row0]
     (range 1 (inc n)))))

(defn- traceback-global [table s1 s2]
  (loop [i (count s1) j (count s2) a1 [] a2 []]
    (if (and (zero? i) (zero? j))
      [(apply str (reverse a1)) (apply str (reverse a2))]
      (let [v (get-in table [i j])]
        (cond
          (and (pos? i) (pos? j)
               (= v (+ (get-in table [(dec i) (dec j)])
                       (sub-score (nth s1 (dec i)) (nth s2 (dec j))))))
          (recur (dec i) (dec j)
                 (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
          (and (pos? i) (= v (+ (get-in table [(dec i) j]) GAP)))
          (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
          :else
          (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j)))))))))

(defn- solve-global [s1 s2]
  (let [table (global-table s1 s2)
        [a1 a2] (traceback-global table s1 s2)]
    [(get-in table [(count s1) (count s2)]) a1 a2]))

(defn- local-table [s1 s2]
  (let [n (count s1)
        m (count s2)
        row0 (vec (repeat (inc m) 0))]
    (reduce
     (fn [[rows score bi bj] i]
       (let [prev (peek rows)
             c1 (nth s1 (dec i))
             [row score bi bj]
             (reduce
              (fn [[r score bi bj] j]
                (let [diag (+ (nth prev (dec j))
                              (sub-score c1 (nth s2 (dec j))))
                      up (+ (nth prev j) GAP)
                      left (+ (peek r) GAP)
                      v (max 0 diag up left)]
                  (if (> v score)
                    [(conj r v) v i j]
                    [(conj r v) score bi bj])))
              [[0] score bi bj]
              (range 1 (inc m)))]
         [(conj rows row) score bi bj]))
     [[row0] 0 0 0]
     (range 1 (inc n)))))

(defn- traceback-local [table s1 s2 bi bj]
  (loop [i bi j bj a1 [] a2 []]
    (if (or (zero? i) (zero? j) (not (pos? (get-in table [i j]))))
      [(apply str (reverse a1)) (apply str (reverse a2))]
      (let [v (get-in table [i j])]
        (cond
          (= v (+ (get-in table [(dec i) (dec j)])
                  (sub-score (nth s1 (dec i)) (nth s2 (dec j)))))
          (recur (dec i) (dec j)
                 (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
          (= v (+ (get-in table [(dec i) j]) GAP))
          (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
          :else
          (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j)))))))))

(defn- solve-local [s1 s2]
  (let [[table score bi bj] (local-table s1 s2)
        [a1 a2] (traceback-local table s1 s2 bi bj)]
    [score a1 a2]))

(let [toks (re-seq #"\S+" (slurp *in*))]
  (when (>= (count toks) 3)
    (let [[mode s1 s2] toks
          [score a1 a2] (if (= mode "GLOBAL")
                          (solve-global s1 s2)
                          (solve-local s1 s2))]
      (println score)
      (println a1)
      (println a2))))
