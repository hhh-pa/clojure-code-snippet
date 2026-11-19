;; Tower of Hanoi move sequence (A -> C via B).
(require '[clojure.string :as str])

(defn- hn-moves
  [k src dst aux]
  (if (zero? k)
    []
    (concat (hn-moves (dec k) src aux dst)
            [(str src " " dst)]
            (hn-moves (dec k) aux dst src))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [n (Long/parseLong (first (str/split (str/trim s) #"\s+")))
          moves (vec (hn-moves n "A" "C" "B"))]
      (print (str (count moves)))
      (doseq [mv moves]
        (print (str "\n" mv))))))
