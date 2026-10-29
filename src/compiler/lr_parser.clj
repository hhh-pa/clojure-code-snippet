(ns compiler.lr-parser
  "Table-driven LR(0)/SLR parser for a fixed expression grammar.

   Grammar:
     E -> E + T | T
     T -> T * F | F
     F -> ( E ) | id

   Time: O(n) shifts/reductions, each O(1) amortized (persistent vector
   push/pop; each token shifted once and each stack symbol popped once).
   Space: O(n) for the state stack and token vector.
   This is asymptotically optimal for deterministic LR parsing."
  (:require [clojure.string :as str]))

(def productions
  "Index by production number; each entry [pretty-string rhs-length]."
  [["" 0]
   ["E -> E + T" 3]
   ["E -> T" 1]
   ["T -> T * F" 3]
   ["T -> F" 1]
   ["F -> ( E )" 3]
   ["F -> id" 1]])

(def action-table
  "Map [state token] -> action. Positive = shift, negative = reduce by
   production (-act), 1000 = accept."
  {[0 "id"] 5, [0 "("] 4
   [1 "+"] 6, [1 "$"] 1000
   [2 "+"] -2, [2 "*"] 7, [2 ")"] -2, [2 "$"] -2
   [3 "+"] -4, [3 "*"] -4, [3 ")"] -4, [3 "$"] -4
   [4 "id"] 5, [4 "("] 4
   [5 "+"] -6, [5 "*"] -6, [5 ")"] -6, [5 "$"] -6
   [6 "id"] 5, [6 "("] 4
   [7 "id"] 5, [7 "("] 4
   [8 "+"] 6, [8 ")"] 11
   [9 "+"] -1, [9 "*"] 7, [9 ")"] -1, [9 "$"] -1
   [10 "+"] -3, [10 "*"] -3, [10 ")"] -3, [10 "$"] -3
   [11 "+"] -5, [11 "*"] -5, [11 ")"] -5, [11 "$"] -5})

(def goto-table
  "Map [state nonterminal] -> next state."
  {[0 "E"] 1, [0 "T"] 2, [0 "F"] 3
   [4 "E"] 8, [4 "T"] 2, [4 "F"] 3
   [6 "T"] 9, [6 "F"] 3
   [7 "F"] 10})

(def accept-action 1000)

(defn read-tokens
  "Split trimmed stdin into whitespace-separated tokens."
  [s]
  (vec (remove str/blank? (str/split (str/trim s) #"\s+"))))

(defn parse-tokens
  "Run the LR automaton over token vector `toks` (with trailing $).
   Prints each reduction, then the accept or syntax-error verdict."
  [toks]
  (loop [state-stack [0]
         ip 0]
    (let [s (peek state-stack)
          a (get toks ip)]
      (if-let [act (get action-table [s a])]
        (cond
          (= act accept-action)
          (println "ACCEPTED")

          (pos? act)
          (recur (conj state-stack act) (inc ip))

          (neg? act)
          (let [prod-idx (- act)
                [prod-str rhs-len] (get productions prod-idx)]
            (println prod-str)
            (let [new-stack (subvec state-stack 0 (- (count state-stack) rhs-len))
                  t (peek new-stack)
                  lhs (subs prod-str 0 1)]
              (if-let [next-state (get goto-table [t lhs])]
                (recur (conj new-stack next-state) ip)
                (println "SYNTAX_ERROR"))))

          :else
          (println "SYNTAX_ERROR"))
        (println "SYNTAX_ERROR")))))

(defn -main
  "Entry point. Blank stdin produces no output. Every non-blank input is a
   valid token stream for this fixed grammar; malformed sentences report
   SYNTAX_ERROR on stdout (matching HEAD)."
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [raw-toks (read-tokens s)]
        (when (seq raw-toks)
          (let [toks (if (= (peek raw-toks) "$")
                       raw-toks
                       (conj raw-toks "$"))]
            (parse-tokens toks)))))))

(-main)
