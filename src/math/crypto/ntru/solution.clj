;; NTRUEncrypt (toy N=7, p=3, q=41).
(require '[clojure.string :as str])

(defn polymul [a b n md]
  (reduce (fn [c [i j]]
            (update c (rem (+ i j) n) #(mod (+ % (* (nth a i) (nth b j))) md)))
          (vec (repeat n 0))
          (for [i (range n) j (range n)] [i j])))

(defn center [v md]
  (let [r (mod v md)] (if (> r (quot md 2)) (- r md) r)))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)
      n (Integer/parseInt (second toks)) p (Long/parseLong (nth toks 2)) q (Long/parseLong (nth toks 3))
      rest (mapv #(Long/parseLong %) (drop 4 toks))
      a (subvec rest 0 n) b (subvec rest n (* 2 n)) c (subvec rest (* 2 n) (* 3 n))]
  (if (= op "ENC")
    (let [t (polymul b a n q)]
      (print (str/join " " (map #(center (+ %1 %2) q) t c))))
    (let [aq (mapv #(mod % q) a) eq (mapv #(mod % q) c)
          t (polymul aq eq n q)
          ap (mapv #(mod (center % q) p) t)]
      (print (str/join " " (polymul b ap n p))))))
