(ns hill-climbing
  (:require [clojure.string :as str]))

(defn attacking-pairs [board]
  (let [n (count board)]
    (count (for [i (range n) j (range (inc i) n)
                 :when (let [bi (nth board i) bj (nth board j)]
                         (or (= bi bj) (= (Math/abs (- bi bj)) (Math/abs (- i j)))))]
             1))))

(defn steepest-step [board]
  (let [n (count board)
        cur (attacking-pairs board)]
    (if (zero? cur) nil
        (let [best (reduce (fn [best [r c]]
                             (let [old (nth board r)
                                   nb (assoc board r c)
                                   v (attacking-pairs nb)]
                               (if (< v (:val best)) {:val v :row r :col c} best)))
                           {:val cur :row -1 :col -1}
                           (for [r (range n) c (range n) :when (not= c (nth board r))] [r c]))]
          (when (not= (:row best) -1) best)))))

(defn hill-climb [board]
  (loop [b board]
    (if-let [step (steepest-step b)]
      (recur (assoc b (:row step) (:col step)))
      b)))

(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        parse #(Long/parseLong %)
        t (parse (first toks))
        nums (mapv parse (rest toks))]
    (loop [pos 0 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [n (nth nums pos)
              board (vec (map int (subvec nums (inc pos) (+ pos 1 n))))
              final-board (hill-climb board)
              ap (attacking-pairs final-board)]
          (.append sb (str/join " " final-board))
          (.append sb "\n")
          (.append sb (str ap))
          (.append sb "\n")
          (recur (+ pos 1 n) (dec tcases) sb))))))

(-main)
