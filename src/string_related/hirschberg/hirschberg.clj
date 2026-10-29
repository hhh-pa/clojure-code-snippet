;; Hirschberg linear space sequence alignment.
;; Input: two whitespace separated strings.
;; Output: alignment score plus the two aligned strings, one per line.
;; Optimal O(n m) time with O(m) space, recursing on halves around a
;; middle split found with two linear scoring passes. Diagonal moves
;; win ties, then up, then left, matching the classic full table.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def match-score 2)
(def mismatch-score -1)
(def gap-score -2)

(defn- blank-char?
  "True for the whitespace chars that split tokens."
  [c]
  (or (= c \space) (= c \tab) (= c \newline) (= c \return) (= c \formfeed)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [c (first cs)]
        (if (blank-char? c)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur c) toks))))))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- nw-score
  "Last row of the global alignment table for char vectors x and y,
  kept in two rows for linear space in the length of y."
  [x y]
  (let [m (count y)
        init (vec (map #(* gap-score %) (range (inc m))))]
    (loop [i 0 prev (transient init)]
      (if (>= i (count x))
        (persistent! prev)
        (let [cx (nth x i)
              seed (transient (vec (repeat (inc m) 0)))
              _ (assoc! seed 0 (+ (nth prev 0) gap-score))
              curr (loop [j 0 curr seed]
                     (if (>= j m)
                       curr
                       (let [s (if (= cx (nth y j)) match-score mismatch-score)
                             v (max (+ (nth prev j) s)
                                    (+ (nth prev (inc j)) gap-score)
                                    (+ (nth curr j) gap-score))]
                         (assoc! curr (inc j) v)
                         (recur (inc j) curr))))]
          (recur (inc i) curr))))))

(defn- nw-base
  "Full table alignment for the single row or column case, with the
  diagonal first traceback."
  [x y]
  (let [n (count x)
        m (count y)
        row0 (vec (map #(* gap-score %) (range (inc m))))
        dp (loop [i 1 dp (transient [row0])]
             (if (> i n)
               (persistent! dp)
               (let [prev (nth dp (dec i))
                     cx (nth x (dec i))
                     row (transient (vec (repeat (inc m) 0)))]
                 (assoc! row 0 (+ (nth prev 0) gap-score))
                 (loop [j 1]
                   (when (<= j m)
                     (let [s (if (= cx (nth y (dec j))) match-score mismatch-score)]
                       (assoc! row j (max (+ (nth prev (dec j)) s)
                                          (+ (nth prev j) gap-score)
                                          (+ (nth row (dec j)) gap-score))))
                     (recur (inc j))))
                 (recur (inc i) (conj! dp (persistent! row))))))]
    (loop [i n j m a1 [] a2 []]
      (if (and (zero? i) (zero? j))
        [(apply str (reverse a1)) (apply str (reverse a2))]
        (cond
          (and (pos? i) (pos? j)
               (= (nth (nth dp i) j)
                  (+ (nth (nth dp (dec i)) (dec j))
                     (if (= (nth x (dec i)) (nth y (dec j)))
                       match-score mismatch-score))))
          (recur (dec i) (dec j)
                 (conj a1 (nth x (dec i)))
                 (conj a2 (nth y (dec j))))
          (and (pos? i)
               (= (nth (nth dp i) j) (+ (nth (nth dp (dec i)) j) gap-score)))
          (recur (dec i) j
                 (conj a1 (nth x (dec i)))
                 (conj a2 \-))
          :else
          (recur i (dec j)
                 (conj a1 \-)
                 (conj a2 (nth y (dec j)))))))))

(defn- hirschberg
  "Linear space alignment of char vectors x and y, returning the two
  aligned strings."
  [x y]
  (cond
    (empty? x) [(apply str (repeat (count y) \-)) (apply str y)]
    (empty? y) [(apply str x) (apply str (repeat (count x) \-))]
    (or (= 1 (count x)) (= 1 (count y))) (nw-base x y)
    :else
    (let [xmid (quot (count x) 2)
          xl (subvec x 0 xmid)
          xr (subvec x xmid)
          score-l (nw-score xl y)
          score-r (nw-score (vec (reverse xr)) (vec (reverse y)))
          m (count y)
          [_ best-j] (reduce (fn [[best bj] j]
                               (let [total (+ (nth score-l j) (nth score-r (- m j)))]
                                 (if (> total best) [total j] [best bj])))
                             [-1000000000 0]
                             (range (inc m)))
          [left-a1 left-a2] (hirschberg xl (subvec y 0 best-j))
          [right-a1 right-a2] (hirschberg xr (subvec y best-j))]
      [(str left-a1 right-a1) (str left-a2 right-a2)])))

(defn- alignment-score
  "Scores two aligned strings with the match and gap values."
  [a1 a2]
  (reduce + (map (fn [c1 c2]
                   (cond (or (= c1 \-) (= c2 \-)) gap-score
                         (= c1 c2) match-score
                         :else mismatch-score))
                 a1 a2)))

(defn -main
  "Parses data and prints the score plus aligned strings.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)]
        (if (< (count toks) 2)
          (report "error: expected two whitespace separated strings")
          (let [[a1 a2] (hirschberg (vec (nth toks 0)) (vec (nth toks 1)))]
            (println (alignment-score a1 a2))
            (println a1)
            (println a2))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
