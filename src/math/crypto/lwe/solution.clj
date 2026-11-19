;; Regev LWE public-key encryption (toy parameters).
(require '[clojure.string :as str])

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)]
  (if (= op "ENC")
    (let [n (Integer/parseInt (second toks)) q (Long/parseLong (nth toks 2))
          mu (Long/parseLong (nth toks 3))
          rest (mapv #(Long/parseLong %) (drop 4 toks))
          m (quot (count rest) (+ n 2))]
      (print
       (str/join " "
        (concat
         (map (fn [j]
                (mod (reduce + 0 (map (fn [i] (* (nth rest (+ (* m n) m i)) (nth rest (+ (* i n) j)))) (range m))) q))
              (range n))
         [(mod (+ (reduce + 0 (map (fn [i] (* (nth rest (+ (* m n) m i)) (nth rest (+ (* m n) i)))) (range m)))
                  (* mu (quot q 2))) q)]))))
    (let [n (Integer/parseInt (second toks)) q (Long/parseLong (nth toks 2))
          rest (mapv #(Long/parseLong %) (drop 3 toks))
          s (subvec rest 0 n) u (subvec rest n (* 2 n)) v (nth rest (* 2 n))
          x (mod (- v (reduce + 0 (map * s u))) q)]
      (print (if (< (min x (- q x)) (/ q 4)) "0" "1")))))
