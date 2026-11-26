;; N-queens enumeration (row-by-row backtracking, lex order).
(require '[clojure.string :as str])

(defn- nq-place
  [n r cur cols d1 d2]
  (if (= r n)
    [(str/join " " cur)]
    (mapcat (fn [c]
              (if (or (contains? cols c) (contains? d1 (- r c)) (contains? d2 (+ r c)))
                []
                (nq-place n (inc r) (conj cur c) (conj cols c)
                          (conj d1 (- r c)) (conj d2 (+ r c)))))
            (range n))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [n (Long/parseLong (first (str/split (str/trim s) #"\s+")))
          sols (nq-place n 0 [] #{} #{} #{})]
      (print (str (count sols)))
      (doseq [ln sols]
        (print (str "\n" ln))))))
