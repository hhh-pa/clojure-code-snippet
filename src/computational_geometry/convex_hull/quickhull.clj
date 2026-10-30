;; 2D Convex Hull by QuickHull (Sedgewick Ch 1).
;;
;; Time: O(n log n) average, O(n^2) worst case.
;; Space: O(n) for point partitions and hull vertices.
;; Purity: core Clojure only, zero host interop.
;; Robustness: blank stdin prints nothing; malformed input prints an error
;;   message to *err* and exits normally.
(ns computational-geometry.convex-hull.quickhull
  (:require [clojure.string :as str]))

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(defn whitespace?
  "True for ASCII whitespace separating input tokens."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)
      (= ch \formfeed) (= ch (char 11))))

(defn tokenize
  "Split s on ASCII whitespace."
  [s]
  (let [[toks cur] (reduce (fn [[toks cur] ch]
                             (if (whitespace? ch)
                               [(if (seq cur) (conj toks (apply str cur)) toks) []]
                               [toks (conj cur ch)]))
                           [[] []]
                           (seq s))]
    (if (seq cur) (conj toks (apply str cur)) toks)))

(defn parse-long-token
  "Parse a signed 64-bit integer token; nil when malformed or out of range."
  [tok]
  (when (some? tok)
    (let [cs (vec tok)
          n (count cs)]
      (when (pos? n)
        (let [c0 (nth cs 0)
              neg (or (= c0 \-) (= c0 \+))
              want-neg (= c0 \-)
              digits (subvec cs (if neg 1 0))]
          (when (and (seq digits) (every? digit-values digits))
            (let [stripped (vec (drop-while #(= % \0) digits))
                  len (count stripped)
                  lim (if want-neg "9223372036854775808" "9223372036854775807")]
              (when (or (< len 19)
                        (and (= len 19) (<= (compare (apply str stripped) lim) 0)))
                (let [v (reduce (fn [acc ch] (- (* acc 10) (digit-values ch)))
                                0 stripped)]
                  (if want-neg v (- v)))))))))))

(defn cross
  "Twice the signed area of triangle o-a-b using arbitrary-precision math."
  [[ox oy] [ax ay] [bx by]]
  (-' (*' (-' ax ox) (-' by oy)) (*' (-' ay oy) (-' bx ox))))

(defn build-hull
  "Recursively find hull vertices strictly outside directed edge p -> q."
  [p q pts]
  (if (empty? pts)
    []
    (let [c (reduce (fn [best pt]
                      (let [db (cross q p best)
                            d (cross q p pt)]
                        (if (or (> d db)
                                (and (= d db) (neg? (compare pt best))))
                          pt
                          best)))
                    (first pts)
                    (rest pts))
          left-pc (vec (filter #(pos? (cross c p %)) pts))
          left-cq (vec (filter #(pos? (cross q c %)) pts))]
      (vec (concat (build-hull p c left-pc)
                   [c]
                   (build-hull c q left-cq))))))

(defn quickhull
  "Compute extreme CCW convex hull vertices starting from smallest (x, y)."
  [raw-pts]
  (let [pts (vec (distinct (sort raw-pts)))
        u (count pts)]
    (if (<= u 2)
      pts
      (let [a (first pts)
            b (peek pts)
            mid (subvec pts 1 (dec u))
            lower-pts (vec (filter #(neg? (cross a b %)) mid))
            upper-pts (vec (filter #(pos? (cross a b %)) mid))]
        (if (and (empty? lower-pts) (empty? upper-pts))
          [a b]
          (let [raw (vec (concat [a]
                                 (build-hull a b lower-pts)
                                 [b]
                                 (build-hull b a upper-pts)))
                m (count raw)
                hull (vec (for [i (range m)
                                :let [prev (nth raw (rem (+ i m -1) m))
                                      curr (nth raw i)
                                      nxt (nth raw (rem (inc i) m))]
                                :when (pos? (cross prev curr nxt))]
                            curr))
                min-idx (first (reduce (fn [best cur]
                                         (if (neg? (compare (second cur) (second best)))
                                           cur best))
                                       (map-indexed vector hull)))]
            (vec (concat (subvec hull min-idx)
                         (subvec hull 0 min-idx)))))))))

(defn die
  "Print error message to *err* and return nil."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (tokenize content)
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer point count")

          (neg? n)
          (die "error: point count must be non-negative")

          (< (count (rest tokens)) (* 2 n))
          (die "error: insufficient coordinate tokens")

          :else
          (let [coords (take (* 2 n) (rest tokens))
                pts (vec (for [[xs ys] (partition 2 coords)]
                           [(parse-long-token xs) (parse-long-token ys)]))]
            (if (some (fn [[x y]] (or (nil? x) (nil? y))) pts)
              (die "error: malformed coordinate token")
              (doseq [[x y] (quickhull pts)]
                (println (str x " " y))))))))))

(-main)
