;; Indexing and ranking (NR 8.4).
(require '[clojure.string :as str])

(def mcut 7)

(defn swapv [v i j]
  (let [t (nth v i)]
    (assoc v i (nth v j) j t)))

(defn indexx [arr]
  (let [n (count arr)]
    (loop [indx (vec (range n)) istack [] ir (dec n) l 0]
      (if (< (- ir l) mcut)
        (let [indx (loop [j (inc l) indx indx]
                     (if (> j ir)
                       indx
                       (let [indxt (nth indx j)
                             a (nth arr indxt)
                             [i indx] (loop [i (dec j) indx indx]
                                        (if (< i l)
                                          [i indx]
                                          (if (<= (nth arr (nth indx i)) a)
                                            [i indx]
                                            (recur (dec i) (assoc indx (inc i) (nth indx i))))))]
                         (recur (inc j) (assoc indx (inc i) indxt)))))]
          (if (empty? istack)
            indx
            (let [cnt (count istack)]
              (recur indx (subvec istack 0 (- cnt 2)) (nth istack (- cnt 1)) (nth istack (- cnt 2))))))
        (let [k (bit-shift-right (+ l ir) 1)
              indx (swapv indx k (inc l))
              indx (if (> (nth arr (nth indx l)) (nth arr (nth indx ir)))
                     (swapv indx l ir)
                     indx)
              indx (if (> (nth arr (nth indx (inc l))) (nth arr (nth indx ir)))
                     (swapv indx (inc l) ir)
                     indx)
              indx (if (> (nth arr (nth indx l)) (nth arr (nth indx (inc l))))
                     (swapv indx l (inc l))
                     indx)
              indxt (nth indx (inc l))
              a (nth arr indxt)
              [i j indx] (loop [i (inc l) j ir indx indx]
                           (let [i (loop [i (inc i)]
                                     (if (< (nth arr (nth indx i)) a) (recur (inc i)) i))
                                 j (loop [j (dec j)]
                                     (if (> (nth arr (nth indx j)) a) (recur (dec j)) j))]
                             (if (< j i)
                               [i j indx]
                               (recur i j (swapv indx i j)))))
              indx (assoc indx (inc l) (nth indx j) j indxt)
              istack (if (>= (+ (- ir i) 1) (- j l))
                       (conj istack i ir)
                       (conj istack l (dec j)))
              [ir l] (if (>= (+ (- ir i) 1) (- j l)) [(dec j) l] [ir i])]
          (recur indx istack ir l))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (when (or (= op "INDEX") (= op "RANK"))
        (let [n (Long/parseLong (nth toks 1))
              arr (vec (map #(Long/parseLong %) (subvec toks 2 (+ 2 n))))
              indx (indexx arr)]
          (println
           (str/join " "
            (if (= op "INDEX")
              (map str indx)
              (map str (let [r (vec (repeat n 0))]
                         (loop [j 0 r r]
                           (if (>= j n) r (recur (inc j) (assoc r (nth indx j) j))))))))))))))
