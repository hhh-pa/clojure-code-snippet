(ns solution
  (:require [clojure.string :as str]))

(defn tandem-repeats [s]
  (let [n (count s) b (vec s)]
    (for [i (range n)
          len (range 1 (inc (quot (- n i) 2)))
          :when (= (subvec b i (+ i len)) (subvec b (+ i len) (+ i (* 2 len))))]
      [i len])))

(defn -main []
  (let [input (slurp *in*)
        toks (filter seq (str/split input #"\s+"))
        s (first toks)]
    (when s
      (let [reps (tandem-repeats s)]
        (println (count reps))
        (doseq [[p l] reps]
          (println p l))))))

(-main)
