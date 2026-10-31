;; turpin_coan: Turpin-Coan multi-valued to binary Byzantine reduction.
;; Correct processes reliably broadcast values (faulty messages arrive
;; inconsistent and are dropped); each correct i sets w_i=v on >=n-f
;; matching values else NONE, with binary input b_i; ideal binary BA
;; decides AND; decision is v iff binary decides 1.
;; Batch 6 (T331). Input: n f, values, faulty list.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn parse-token [t] (Long/parseLong t))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [toks (mapv parse-token toks)
            n (nth toks 0) f (nth toks 1)
            v (subvec toks 2 (+ 2 n))
            k (nth toks (+ 2 n))
            fl (subvec toks (+ 3 n) (+ 3 n k))
            faulty (reduce (fn [a x] (assoc a x true)) (vec (repeat n false)) fl)
            cv (filterv (fn [j] (not (nth faulty j))) (range n))
            fr (frequencies (mapv #(nth v %) cv))
            best (reduce (fn [[bv bc] x] (let [c (get fr x 0)] (if (> c bc) [x c] [bv bc])))
                         [0 0] (sort (keys fr)))
            [bv bc] best
            ok (>= bc (- n f))
            band (or (empty? cv) ok)
            decide (and band (seq cv) ok)]
        (doseq [p (range n)]
          (if (nth faulty p) (println (str "P" p ": FAULTY"))
            (println (str "P" p ": w=" (if ok bv "NONE") " b=" (if ok 1 0)))))
        (println (str "BINARY: " (if band 1 0)))
        (println (str "DECISION: " (if decide bv "ABSTAIN")))))))
(-main (slurp *in*))
