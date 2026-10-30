
;; recursive_descent: predictive recursive-descent parser and evaluator
;; for integer arithmetic. Mutually recursive procedures per grammar
;; rule with single-token lookahead; truncating division.
(ns recursive-descent
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(declare parse-e)

(defn parse-f [s p]
  (if (>= p (count s))
    [0 p true]
    (let [c (.charAt s p)]
      (cond (= c \()
            (let [[v p2 e2] (parse-e s (inc p))]
              (if (or e2 (>= p2 (count s)) (not= (.charAt s p2) \)))
                [0 p2 true]
                [v (inc p2) false]))
            (= c \-)
            (let [[v p2 e2] (parse-f s (inc p))]
              [(- v) p2 e2])
            (and (>= (int c) (int \0)) (<= (int c) (int \9)))
            (loop [i p v 0]
              (if (and (< i (count s))
                       (let [d (.charAt s i)]
                         (and (>= (int d) (int \0)) (<= (int d) (int \9)))))
                (recur (inc i) (+ (* v 10) (- (int (.charAt s i)) (int \0))))
                [v i false]))
            :else [0 p true]))))

(defn parse-t [s p]
  (let [[v p2 e2] (parse-f s p)]
    (if e2
      [0 p2 true]
      (loop [acc v i p2]
        (if (or (>= i (count s)) (not (contains? #{\* \/} (.charAt s i))))
          [acc i false]
          (let [op (.charAt s i)
                [r p3 e3] (parse-f s (inc i))]
            (cond e3 [0 p3 true]
                  (and (= op \/) (zero? r)) [0 p3 true]
                  :else (recur (if (= op \*) (* acc r) (quot acc r)) p3))))))))

(defn parse-e [s p]
  (let [[v p2 e2] (parse-t s p)]
    (if e2
      [0 p2 true]
      (loop [acc v i p2]
        (if (or (>= i (count s)) (not (contains? #{\+ \-} (.charAt s i))))
          [acc i false]
          (let [op (.charAt s i)
                [r p3 e3] (parse-t s (inc i))]
            (if e3
              [0 p3 true]
              (recur (if (= op \+) (+ acc r) (- acc r)) p3))))))))

(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (loop [pos 1 tcases (parse-token (first toks)) sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [e (nth toks pos)
              [v p2 er] (parse-e e 0)]
          (.append sb (if (or er (not= p2 (count e))) "RD ERROR\n" (str "RD " v "\n")))
          (recur (inc pos) (dec tcases) sb))))))

(-main)
