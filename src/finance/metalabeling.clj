;; Meta-labeling: correctness + fixed-tree size (AFML ch 3).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          e (Long/parseLong (nth toks 0))
          f (Long/parseLong (nth toks 1))
          sides (mapv #(Long/parseLong (nth toks (+ 2 (* (+ 2 f) %)))) (range e))
          feats (mapv (fn [i]
                        (mapv #(Double/parseDouble (nth toks (+ 3 (* (+ 2 f) i) %))) (range f)))
                      (range e))
          labels (mapv #(Long/parseLong (nth toks (+ 3 f (* (+ 2 f) %)))) (range e))
          t0 (+ 2 (* (+ 2 f) e))
          t (Long/parseLong (nth toks t0))
          nodes (loop [i 0 idx (inc t0) acc []]
                  (if (>= i t)
                    acc
                    (if (= (nth toks idx) "LEAF")
                      (recur (inc i) (+ idx 2)
                             (conj acc [:leaf (Double/parseDouble (nth toks (inc idx)))]))
                      (recur (inc i) (+ idx 5)
                             (conj acc [:split (Long/parseLong (nth toks (inc idx)))
                                        (Double/parseDouble (nth toks (+ idx 2)))
                                        (Long/parseLong (nth toks (+ idx 3)))
                                        (Long/parseLong (nth toks (+ idx 4)))])))))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          [out good] (reduce (fn [[os gd] i]
                               (let [y (if (and (== (nth sides i) (nth labels i))
                                                (not (== (nth labels i) 0)))
                                         1 0)
                                     node (loop [nd 0]
                                            (let [[kind a b c dd] (nth nodes nd)]
                                              (if (= kind :leaf)
                                                nd
                                                (recur (if (<= (nth (nth feats i) a) b) c dd)))))]
                                 [(conj os (str "META " y " SIZE " (fmt (second (nth nodes node)))))
                                  (+ gd y)]))
                             [[] 0] (range e))]
      (print (str (str/join "\n" out) "\nACC " (fmt (/ (double good) e)))))))
