(let [prods [["" 0]
             ["E -> E + T" 3]
             ["E -> T" 1]
             ["T -> T * F" 3]
             ["T -> F" 1]
             ["F -> ( E )" 3]
             ["F -> id" 1]]
      action-table {[0 "id"] 5, [0 "("] 4
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
                    [11 "+"] -5, [11 "*"] -5, [11 ")"] -5, [11 "$"] -5}
      goto-table {[0 "E"] 1, [0 "T"] 2, [0 "F"] 3
                  [4 "E"] 8, [4 "T"] 2, [4 "F"] 3
                  [6 "T"] 9, [6 "F"] 3
                  [7 "F"] 10}
      s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [raw-toks (vec (.split (.trim ^String s) "\\s+"))
          toks (if (= (peek raw-toks) "$") raw-toks (conj raw-toks "$"))]
      (loop [state-stack [0]
             ip 0]
        (let [s (peek state-stack)
              a (get toks ip)]
          (if-let [act (get action-table [s a])]
            (cond
              (= act 1000) (println "ACCEPTED")
              (pos? act) (recur (conj state-stack act) (inc ip))
              (neg? act) (let [prod-idx (- act)
                               [prod-str len] (get prods prod-idx)
                               _ (println prod-str)
                               new-stack (subvec state-stack 0 (- (count state-stack) len))
                               t (peek new-stack)
                               lhs (subs prod-str 0 1)]
                           (if-let [next-state (get goto-table [t lhs])]
                             (recur (conj new-stack next-state) ip)
                             (println "SYNTAX_ERROR")))
              :else (println "SYNTAX_ERROR"))
            (println "SYNTAX_ERROR")))))))
