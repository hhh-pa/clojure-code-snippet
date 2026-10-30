;; knuth-div: multi-precision long division on decimal strings
;; (schoolbook long division in the spirit of Knuth's Algorithm D).
;; Input: A B as decimal strings. Output: quotient, remainder.
(ns knuth-div (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 10) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn strip0 [s]
  (let [t (str/replace s #"^0+" "")]
    (if (str/blank? t) "0" t)))
(defn cmpn [a b]
  (let [la (count a) lb (count b)]
    (cond (< la lb) -1 (> la lb) 1 (= a b) 0
          (neg? (compare a b)) -1 :else 1)))
(defn subn [a b] ; requires a >= b
  (loop [i (dec (count a)) j (dec (count b)) carry 0 acc ()]
    (if (neg? i) (strip0 (apply str acc))
      (let [d (- (Character/digit (nth a i) 10) carry
                 (if (neg? j) 0 (Character/digit (nth b j) 10)))
            dd (if (neg? d) (+ d 10) d)]
        (recur (dec i) (dec j) (if (neg? d) 1 0) (cons (char (+ dd 48)) acc))))))
(defn divmod-str [A B]
  (if (neg? (cmpn A B)) ["0" A]
    (loop [k 0 cur "0" q []]
      (if (>= k (count A)) [(strip0 (apply str q)) cur]
        (let [c1 (strip0 (str cur (nth A k)))
              [c2 cnt] (loop [c c1 cnt 0]
                         (if (neg? (cmpn c B)) [c cnt]
                           (recur (subn c B) (inc cnt))))]
          (recur (inc k) c2 (conj q (char (+ cnt 48)))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [[q r] (divmod-str (strip0 (nth toks 0)) (strip0 (nth toks 1)))]
        (println q)
        (println r)))))
(-main)
