(require '[clojure.string :as str])
;; banded_alignment: Banded alignment: k-difference global alignment in O(kn) banded DP.
;; Only cells with |i-j| <= k are visited, like the python twin; rows are
;; transient vectors so each cell update is O(1).

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

(defn -main [input]
  (let [toks (filter seq (str/split input #"\s+"))
        s1 (nth toks 0 nil) s2 (nth toks 1 nil) k (when (nth toks 2 nil) (Integer/parseInt (nth toks 2)))]
    (when (and s1 s2 k)
      (let [n (count s1) m (count s2) INF 1000000000
            dp (vec (repeatedly (inc n) #(transient (vec (repeat (inc m) INF)))))]
        (assoc! (nth dp 0) 0 0)
        (doseq [i (range (inc n))
                :let [lo (max 0 (- i k)) hi (min m (+ i k))]
                j (range lo (inc hi))]
          (when-not (and (= i 0) (= j 0))
            (let [cands (cond-> []
                          (and (> i 0) (<= (Math/abs (- (dec i) j)) k)) (conj (+ (nth (nth dp (dec i)) j) 1))
                          (and (> j 0) (<= (Math/abs (- i (dec j))) k)) (conj (+ (nth (nth dp i) (dec j)) 1))
                          (and (> i 0) (> j 0) (<= (Math/abs (- (dec i) (dec j))) k)) (conj (+ (nth (nth dp (dec i)) (dec j)) (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1))))
                  best (if (seq cands) (apply min cands) INF)]
              (when (< best INF) (assoc! (nth dp i) j best)))))
        (let [ans (nth (nth dp n) m)] (println (if (>= ans INF) -1 ans)))))))
(-main (slurp *in*))