;; distributed_mutex: distributed mutual exclusion, two protocols selected
;; by the leading mode token. ra: Ricart-Agrawala, grants in (timestamp,id)
;; order with 2(n-1) messages per entry. sk: Suzuki-Kasami token-based with
;; request numbers (RN/LN) and a FIFO token queue. Batch 6 (T341).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn run-ra [toks n]
  (let [R (Long/parseLong (nth toks 2))]
    (loop [r 0 p 3 order []]
      (if (>= r R)
        order
        (let [c (Long/parseLong (nth toks p))
              g (sort (mapv (fn [k] (Long/parseLong (nth toks (+ p 1 k)))) (range c)))]
          (recur (inc r) (+ p 1 c) (into order g)))))))
(defn drain [holder ln rn qq qh order hops n]
  (loop [h holder l ln rr rn q qh o order hp hops]
    (if (>= q (count qq))
      [h l rr q o hp]
      (let [j (nth qq q)]
        (if (<= (get-in rr [h j]) (nth l j))
          (recur h l rr (inc q) o hp)
          (let [l2 (assoc l j (get-in rr [h j]))
                r2 (loop [t 0 a rr]
                      (if (>= t n) a (recur (inc t) (assoc-in a [j t] (max (get-in a [j t]) (get-in a [h t]))))))]
            (recur j l2 r2 (inc q) (conj o j) (inc hp))))))))
(defn run-sk [toks n]
  (let [holder0 (Long/parseLong (nth toks 2))
        Q (Long/parseLong (nth toks 3))
        req (mapv (fn [k] (Long/parseLong (nth toks (+ 4 k)))) (range Q))]
    (loop [q 0 holder holder0 ln (vec (repeat n 0))
           rn (vec (repeat n (vec (repeat n 0))))
           queue [] qh 0 order [] hops 0]
      (if (>= q Q)
        [order hops]
        (let [ri (nth req q)
              rn1 (assoc-in rn [ri ri] (inc (get-in rn [ri ri])))
              rnb (loop [t 0 a rn1]
                    (if (>= t n) a (recur (inc t) (assoc-in a [t ri] (get-in rn1 [ri ri])))))]
          (if (= ri holder)
            (recur (inc q) holder (assoc ln ri (get-in rnb [ri ri])) rnb queue qh (conj order ri) hops)
            (let [qq (conj queue ri)
                  res (drain holder ln rnb qq qh order hops n)]
              (recur (inc q) (nth res 0) (nth res 1) (nth res 2) qq (nth res 3) (nth res 4) (nth res 5)))))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [mode (nth toks 0) n (Long/parseLong (nth toks 1))]
        (if (= mode "ra")
          (let [order (run-ra toks n)]
            (println (str "ORDER:" (apply str (map (fn [x] (str " " x)) order))))
            (println (str "MSGS: " (* 2 (- n 1) (count order)))))
          (let [res (run-sk toks n) order (nth res 0) hops (nth res 1)
                Q (Long/parseLong (nth toks 3))]
            (println (str "ORDER:" (apply str (map (fn [x] (str " " x)) order))))
            (println (str "HOPS: " hops))
            (println (str "MSGS: " (+ (* (- n 1) Q) hops)))))))))
(-main (slurp *in*))
