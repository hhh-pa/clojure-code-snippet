;; Hirschberg linear-space sequence alignment. Port of
;; cpp/src/string_related/hirschberg/hirschberg.cc.
;; Input: two whitespace-separated strings. Output: alignment score plus
;; the two aligned strings, one per line.
(def match-score 2)
(def mismatch-score -1)
(def gap-score -2)

(defn- nw-score
  "Last row of the Needleman-Wunsch DP table for x against y."
  [^String x ^String y]
  (let [n (count x)
        m (count y)
        prev (int-array (inc m))
        curr (int-array (inc m))]
    (loop [j 1]
      (when (<= j m)
        (aset-int prev j (int (+ (aget prev (dec j)) gap-score)))
        (recur (inc j))))
    (loop [i 1]
      (when (<= i n)
        (aset-int curr 0 (int (+ (aget prev 0) gap-score)))
        (loop [j 1]
          (when (<= j m)
            (let [s (if (= (.charAt x (dec i)) (.charAt y (dec j)))
                      match-score mismatch-score)
                  diag (int (+ (aget prev (dec j)) s))
                  up (int (+ (aget prev j) gap-score))
                  left (int (+ (aget curr (dec j)) gap-score))]
              (aset-int curr j (int (max diag up left))))
            (recur (inc j))))
        (System/arraycopy curr 0 prev 0 (inc m))
        (recur (inc i))))
    (vec prev)))

(defn- nw-base
  "Full Needleman-Wunsch alignment for the single-row/column base case."
  [^String x ^String y]
  (let [n (count x)
        m (count y)
        dp (vec (repeatedly (inc n) #(int-array (inc m))))]
    (loop [i 1]
      (when (<= i n)
        (aset-int (dp i) 0 (int (+ (aget (dp (dec i)) 0) gap-score)))
        (recur (inc i))))
    (loop [j 1]
      (when (<= j m)
        (aset-int (dp 0) j (int (+ (aget (dp 0) (dec j)) gap-score)))
        (recur (inc j))))
    (loop [i 1]
      (when (<= i n)
        (loop [j 1]
          (when (<= j m)
            (let [s (if (= (.charAt x (dec i)) (.charAt y (dec j)))
                      match-score mismatch-score)
                  diag (int (+ (aget (dp (dec i)) (dec j)) s))
                  up (int (+ (aget (dp (dec i)) j) gap-score))
                  left (int (+ (aget (dp i) (dec j)) gap-score))]
              (aset-int (dp i) j (int (max diag up left))))
            (recur (inc j))))
        (recur (inc i))))
    (loop [i n j m a1 [] a2 []]
      (if (and (zero? i) (zero? j))
        [(apply str (reverse a1)) (apply str (reverse a2))]
        (cond
          (and (pos? i) (pos? j)
               (= (aget (dp i) j)
                  (+ (aget (dp (dec i)) (dec j))
                     (if (= (.charAt x (dec i)) (.charAt y (dec j)))
                       match-score mismatch-score))))
          (recur (dec i) (dec j)
                 (conj a1 (.charAt x (dec i)))
                 (conj a2 (.charAt y (dec j))))
          (and (pos? i)
               (= (aget (dp i) j) (+ (aget (dp (dec i)) j) gap-score)))
          (recur (dec i) j
                 (conj a1 (.charAt x (dec i)))
                 (conj a2 \-))
          :else
          (recur i (dec j)
                 (conj a1 \-)
                 (conj a2 (.charAt y (dec j)))))))))

(defn- hirschberg
  "Linear-space alignment of x and y; returns [aligned-x aligned-y]."
  [^String x ^String y]
  (cond
    (empty? x) [(apply str (repeat (count y) \-)) y]
    (empty? y) [x (apply str (repeat (count x) \-))]
    (or (= 1 (count x)) (= 1 (count y))) (nw-base x y)
    :else
    (let [xmid (quot (count x) 2)
          x-left (subs x 0 xmid)
          x-right (subs x xmid)
          rev-x-right (apply str (reverse x-right))
          rev-y (apply str (reverse y))
          score-l (nw-score x-left y)
          score-r (nw-score rev-x-right rev-y)
          m (count y)
          [_ best-j] (reduce (fn [[best bj] j]
                               (let [total (+ (score-l j) (score-r (- m j)))]
                                 (if (> total best) [total j] [best bj])))
                             [-1000000000 0]
                             (range (inc m)))
          [left-a1 left-a2] (hirschberg x-left (subs y 0 best-j))
          [right-a1 right-a2] (hirschberg x-right (subs y best-j))]
      [(str left-a1 right-a1) (str left-a2 right-a2)])))

(defn- alignment-score [^String a1 ^String a2]
  (reduce + (map (fn [c1 c2]
                   (cond (or (= c1 \-) (= c2 \-)) gap-score
                         (= c1 c2) match-score
                         :else mismatch-score))
                 a1 a2)))

(let [toks (re-seq #"\S+" (slurp *in*))]
  (when (>= (count toks) 2)
    (let [[s1 s2] toks
          [a1 a2] (hirschberg s1 s2)]
      (println (alignment-score a1 a2))
      (println a1)
      (println a2))))
