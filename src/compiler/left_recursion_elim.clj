
;; left_recursion_elim: CFG left-recursion elimination (ordered
;; substitution for indirect recursion plus immediate elimination)
;; followed by iterated longest-prefix left factoring.
(ns left-recursion-elim
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn syms-of [s]
  (loop [i 0 acc []]
    (if (>= i (count s))
      acc
      (let [c (.charAt s i)]
        (if (and (>= (int c) (int \A)) (<= (int c) (int \Z)))
          (let [j (loop [k (inc i)]
                    (if (and (< k (count s)) (= (.charAt s k) \'))
                      (recur (inc k)) k))]
            (recur j (conj acc (subs s i j))))
          (recur (inc i) (conj acc (str c))))))))

(defn show-alt [a]
  (if (empty? a) "@" (str/join "" a)))

(defn fresh-for [base rules]
  (loop [b (str base "'")]
    (if (some #(= (:lhs %) b) rules)
      (recur (str b "'"))
      b)))

(defn subst-rule [rules i j]
  (let [ri (nth rules i) rj (nth rules j)]
    (assoc rules i
           (assoc ri :alts
                  (vec (mapcat (fn [a]
                                 (if (and (seq a) (= (first a) (:lhs rj)))
                                   (map (fn [b] (vec (concat b (rest a)))) (:alts rj))
                                   [a]))
                               (:alts ri)))))))

(defn dedup-alts [alts]
  (reduce (fn [acc a] (if (some #(= % a) acc) acc (conj acc a))) [] alts))

(defn elim-immediate [rules i]
  (let [ri (nth rules i)
        rec (filterv #(and (seq %) (= (first %) (:lhs ri))) (:alts ri))
        nonrec (filterv #(not (and (seq %) (= (first %) (:lhs ri)))) (:alts ri))]
    (if (empty? rec)
      rules
      (let [np (fresh-for (:lhs ri) rules)
            nalts (if (empty? nonrec)
                    [[np]]
                    (mapv #(conj (vec %) np) nonrec))
            palts (vec (concat (mapv #(conj (vec (rest %)) np) rec)
                               (if (seq nonrec) [[]] [])))]
        (-> rules
            (assoc i (assoc ri :alts nalts))
            (conj {:lhs np :alts palts}))))))

(defn common-prefix [g]
  (loop [p 0]
    (if (or (>= p (count (first g)))
            (some #(or (>= p (count %)) (not= (nth % p) (nth (first g) p))) (rest g)))
      p
      (recur (inc p)))))

(defn factor-once [rules]
  (let [found (first (for [i (range (count rules))
                           :let [ri (nth rules i)
                                 groups (group-by #(let [a (second %)] (if (empty? a) "" (first a)))
                                                  (map-indexed vector (:alts ri)))]
                           k (sort (keys groups))
                           :let [v (get groups k)]
                           :when (>= (count v) 2)
                           :let [g (mapv #(nth (:alts ri) (first %)) v)
                                 pre (common-prefix g)]
                           :when (pos? pre)]
                       [i k v pre]))]
    (if (nil? found)
      [rules false]
      (let [[i _ v pre] found
            ri (nth rules i)
            np (fresh-for (:lhs ri) rules)
            prefix (vec (take pre (nth (:alts ri) (first (first v)))))
            inset (set (map first v))
            first-idx (first (first v))
            first-new (conj prefix np)
            keep (vec (mapcat (fn [[kk a]]
                                (if (contains? inset kk)
                                  (if (= kk first-idx) [first-new] [])
                                  [a]))
                              (map-indexed vector (:alts ri))))
            suflist (mapv #(vec (drop pre (nth (:alts ri) (first %)))) v)]
        [(-> rules
             (assoc i (assoc ri :alts keep))
             (conj {:lhs np :alts suflist}))
         true]))))

(defn solve-one [rules]
  (let [n0 (count rules)
        r1 (loop [i 0 rs rules]
             (if (= i n0)
               rs
               (let [rs2 (reduce (fn [acc j] (subst-rule acc i j)) rs (range i))
                     ri (nth rs2 i)
                     rs3 (assoc rs2 i (assoc ri :alts (dedup-alts (:alts ri))))]
                 (recur (inc i) (elim-immediate rs3 i)))))]
    (loop [rs r1 round 0]
      (if (>= round 20)
        rs
        (let [[rs2 any] (factor-once rs)]
          (if any (recur rs2 (inc round)) rs2))))))

(defn parse-case [toks pos]
  (let [r (Integer/parseInt (nth toks pos))]
    (loop [i 0 p (inc pos) rules []]
      (if (= i r)
        [rules p]
        (let [line (nth toks p)
              eq (.indexOf line "=")
              lhs (subs line 0 eq)
              alts (mapv (fn [a] (if (or (= a "@") (empty? a)) [] (syms-of a)))
                         (str/split (subs line (inc eq)) #"\|" -1))]
          (recur (inc i) (inc p) (conj rules {:lhs lhs :alts alts})))))))

(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (loop [pos 1 tcases (parse-token (first toks)) sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[rules p2] (parse-case toks pos)
              out (solve-one rules)]
          (.append sb (str "GRAMMAR " (count out) "\n"))
          (doseq [rl out]
            (.append sb (str (:lhs rl) "=" (str/join "|" (map show-alt (:alts rl))) "\n")))
          (recur p2 (dec tcases) sb))))))

(-main)
