
;; knn_naive_bayes: k-nearest neighbors (squared Euclidean distance,
;; index tie-break, smallest-label vote ties) and categorical Naive
;; Bayes with Laplace smoothing in the log domain.
(ns knn-naive-bayes
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn knn-pred [x y z k]
  (let [d (sort-by (juxt first second)
                   (map-indexed (fn [i xi]
                                  [(reduce + 0 (map (fn [a b] (let [dd (- a b)] (* dd dd))) z xi)) i])
                                x))
        votes (frequencies (map #(nth y (second %)) (take k d)))
        best (apply max (vals votes))]
    (first (sort (for [[lab c] votes :when (= c best)] lab)))))

(defn nb-score [x y z c n cc vmax]
  (+ (Math/log (/ (double cc) (double n)))
     (loop [f 0 acc 0.0]
       (if (= f (count z))
         acc
         (let [cnt (loop [i 0 k 0]
                     (if (= i n)
                       k
                       (recur (inc i) (if (and (= (nth y i) c)
                                                (= (nth (nth x i) f) (nth z f)))
                                           (inc k) k))))]
           (recur (inc f) (+ acc (Math/log (/ (double (inc cnt))
                                              (double (+ cc (nth vmax f) 1)))))))))))

(defn nb-score [x y z c n cc vmax]
  (+ (Math/log (/ (double cc) (double n)))
     (loop [f 0 acc 0.0]
       (if (= f (count z))
         acc
         (let [cnt (loop [i 0 k 0]
                     (if (= i n)
                       k
                       (recur (inc i) (if (and (= (nth y i) c)
                                                (= (nth (nth x i) f) (nth z f)))
                                           (inc k) k))))]
           (recur (inc f) (+ acc (Math/log (/ (double (inc cnt))
                                              (double (+ cc (nth vmax f) 1)))))))))))

(defn nb-pred [x y z classes vmax]
  (let [n (count x)]
    (loop [cs classes bl (first classes) bs -1e300]
      (if (empty? cs)
        bl
        (let [c (first cs)
              cc (count (filter #(= % c) y))
              sc (nb-score x y z c n cc vmax)]
          (if (> sc (+ bs 1e-12))
            (recur (rest cs) c sc)
            (recur (rest cs) bl bs)))))))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos)) f (int (nth nums (+ pos 1))) k (int (nth nums (+ pos 2)))
        p0 (+ pos 3)
        x (vec (for [i (range n)] (vec (map int (subvec nums (+ p0 (* i (inc f))) (+ p0 (* i (inc f)) f))))))
        y (vec (for [i (range n)] (int (nth nums (+ p0 (* i (inc f)) f)))))
        p1 (+ p0 (* n (inc f)))
        m (int (nth nums p1))
        p2 (inc p1)
        z (vec (for [i (range m)] (vec (map int (subvec nums (+ p2 (* i f)) (+ p2 (* i f) f))))))]
    [x y k z (+ p2 (* m f))]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[x y k z p2] (parse-case nums pos)
              classes (sort (distinct y))
              vmax (vec (for [f (range (count (first x)))] (apply max (map #(nth % f) x))))
              kp (mapv #(knn-pred x y % k) z)
              nb (mapv #(nb-pred x y % classes vmax) z)]
          (.append sb (str "KNN " (str/join " " kp) "\n"))
          (.append sb (str "NB " (str/join " " nb) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
