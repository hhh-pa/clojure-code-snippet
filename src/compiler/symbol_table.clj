
;; T1206 Batch 5 - 312 symbol_table : lexical scopes + activation display.
;; Input: N commands (ENTER EXIT DECLARE LOOKUP ASSIGN DISPLAY).
;; Output: per-command resolution lines + SYMTAB declares/errors.
(ns symbol-table
  (:require [clojure.string :as str]))
(def separators #{\space \tab \return})
(defn digit-values [ch] (- (int ch) (int \0)))
(defn tokenize [line]
  (loop [i 0 cur "" toks []]
    (if (>= i (count line))
      (if (empty? cur) toks (conj toks cur))
      (let [c (.charAt line i)]
        (if (contains? separators c)
          (recur (inc i) "" (if (empty? cur) toks (conj toks cur)))
          (recur (inc i) (str cur c) toks))))))
(defn parse-token [s]
  (loop [i 0 v 0]
    (if (>= i (count s)) v (recur (inc i) (+ (* v 10) (digit-values (.charAt s i)))))))
(defn lookup-in [scopes nm]
  (loop [d (dec (count scopes))]
    (if (< d 0) nil
      (let [ty (get (nth scopes d) nm)]
        (if (nil? ty) (recur (dec d)) [ty d])))))
(defn -main []
  (let [lines (str/split-lines (slurp *in*))
        n (parse-token (first lines))]
    (loop [li 1 done 0 scopes [{}] chain [0] nextid 1 declares 0 errors 0 sb (StringBuilder.)]
      (if (or (>= done n) (>= li (count lines)))
        (do (.append sb (str "SYMTAB declares=" declares " errors=" errors "\n")) (print (str sb)))
        (let [t (tokenize (nth lines li))]
          (if (empty? t)
            (recur (inc li) done scopes chain nextid declares errors sb)
            (let [c (first t)]
              (cond (= c "ENTER")
                    (do (.append sb (str "ENTER " (count scopes) "\n"))
                        (recur (inc li) (inc done) (conj scopes {}) (conj chain nextid) (inc nextid) declares errors sb))
                    (= c "EXIT")
                    (if (<= (count scopes) 1)
                      (do (.append sb "EXIT UNDERFLOW\n")
                          (recur (inc li) (inc done) scopes chain nextid declares (inc errors) sb))
                      (do (.append sb (str "EXIT " (- (count scopes) 2) "\n"))
                          (recur (inc li) (inc done) (pop scopes) (pop chain) nextid declares errors sb)))
                    (= c "DECLARE")
                    (if (contains? (peek scopes) (second t))
                      (do (.append sb (str "DECLARE " (second t) " " (nth t 2) " REDECLARED\n"))
                          (recur (inc li) (inc done) scopes chain nextid declares (inc errors) sb))
                      (do (.append sb (str "DECLARE " (second t) " " (nth t 2) " OK\n"))
                          (recur (inc li) (inc done) (assoc scopes (dec (count scopes)) (assoc (peek scopes) (second t) (nth t 2))) chain nextid (inc declares) errors sb)))
                    (or (= c "LOOKUP") (= c "ASSIGN"))
                    (let [r (lookup-in scopes (second t))]
                      (if (nil? r)
                        (do (.append sb (str c " " (second t) " UNDECLARED\n"))
                            (recur (inc li) (inc done) scopes chain nextid declares (inc errors) sb))
                        (do (.append sb (if (= c "LOOKUP")
                                          (str "LOOKUP " (second t) " " (first r) " " (second r) "\n")
                                          (str "ASSIGN " (second t) " OK " (second r) "\n")))
                            (recur (inc li) (inc done) scopes chain nextid declares errors sb))))
                    (= c "DISPLAY")
                    (do (.append sb "DISPLAY")
                        (loop [d 0]
                          (when (< d (count chain))
                            (.append sb (str " " d ":" (nth chain d)))
                            (recur (inc d))))
                        (.append sb "\n")
                        (recur (inc li) (inc done) scopes chain nextid declares errors sb))
                    :else (recur (inc li) (inc done) scopes chain nextid declares errors sb)))))))))
(-main)
