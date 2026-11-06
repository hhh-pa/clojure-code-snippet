;; cache-simulator: set-associative cache, LRU eviction (S,E,B).
;; Input: S E B n; n byte addresses. Output: "hits misses evictions".
;; Pure Clojure.

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
  (let [S (read-int (nth toks 0)) E (read-int (nth toks 1))
        B (read-int (nth toks 2)) n (read-int (nth toks 3))
        addrs (mapv (fn [i] (read-int (nth toks (+ 4 i)))) (range n))]
    (loop [i 0 sets (vec (repeat S [])) hits 0 misses 0 evict 0]
      (if (>= i n) (str hits " " misses " " evict)
        (let [addr (nth addrs i)
              block (quot addr B)
              set (mod block S)
              tag (quot block S)
              lines (nth sets set)
              at (loop [k 0] (cond (>= k (count lines)) -1
                                   (= (nth lines k) tag) k
                                   :else (recur (+ k 1))))]
          (if (>= at 0)
            (recur (+ i 1)
                   (assoc sets set (vec (cons tag (concat (subvec lines 0 at) (subvec lines (+ at 1))))))
                   (+ hits 1) misses evict)
            (let [trimmed (if (>= (count lines) E) (pop lines) lines)]
              (recur (+ i 1) (assoc sets set (vec (cons tag trimmed)))
                     hits (+ misses 1) (if (>= (count lines) E) (+ evict 1) evict)))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
