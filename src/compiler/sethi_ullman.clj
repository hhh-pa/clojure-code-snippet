
;; T1206 Batch 5 - 316 sethi_ullman : optimal register numbering + eval order.
;; Input: T parenthesized binary expressions, one per line.
;; Output: per case SU number + ORDER evaluation sequence.
(ns sethi-ullman
  (:require [clojure.string :as str]))
(def separators #{\space \tab \return \( \)})
(defn digit-values [ch] (- (int ch) (int \0)))
(defn tokenize [line]
  (loop [i 0 cur "" toks []]
    (if (>= i (count line))
      (if (empty? cur) toks (conj toks cur))
      (let [c (.charAt line i)]
        (cond (= c \() (recur (inc i) "" (conj (if (empty? cur) toks (conj toks cur)) "("))
              (= c \)) (recur (inc i) "" (conj (if (empty? cur) toks (conj toks cur)) ")"))
              (contains? separators c) (recur (inc i) "" (if (empty? cur) toks (conj toks cur)))
              :else (recur (inc i) (str cur c) toks))))))
(defn parse-token [s]
  (loop [i 0 v 0]
    (if (>= i (count s)) v (recur (inc i) (+ (* v 10) (digit-values (.charAt s i)))))))
(defn parse-t [toks p]
  (if (= (nth toks p) "(")
    (let [op (nth toks (inc p))
          [L p2] (parse-t toks (+ p 2))
          [R p3] (parse-t toks p2)
          su (if (= (:su L) (:su R)) (inc (:su L)) (max (:su L) (:su R)))]
      [{:leaf false :label op :l L :r R :su su} (inc p3)])
    [{:leaf true :label (nth toks p) :su 1} (inc p)]))
(defn emit-order [n out]
  (if (:leaf n)
    (conj out (:label n))
    (let [o2 (if (> (:su (:r n)) (:su (:l n)))
               (emit-order (:r n) out)
               (emit-order (:l n) out))
          o3 (if (> (:su (:r n)) (:su (:l n)))
               (emit-order (:l n) o2)
               (emit-order (:r n) o2))]
      (conj o3 (:label n)))))
(defn -main []
  (let [lines (str/split-lines (slurp *in*))
        T (parse-token (first lines))]
    (loop [li 1 tc T sb (StringBuilder.)]
      (if (zero? tc)
        (print (str sb))
        (let [toks (vec (tokenize (nth lines li)))
              [r _] (parse-t toks 0)
              o (emit-order r [])]
          (.append sb (str "SU " (:su r) "\nORDER"))
          (loop [i 0]
            (when (< i (count o))
              (.append sb (str " " (nth o i)))
              (recur (inc i))))
          (.append sb "\n")
          (recur (inc li) (dec tc) sb))))))
(-main)
