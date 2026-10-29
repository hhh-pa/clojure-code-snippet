(ns compiler.earley-parser
  (:require [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn terminal-symbol? [c]
  (and (or (< (int c) (int \A)) (> (int c) (int \Z)))
       (not= c \#)))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [num-rules (Long/parseLong (first tokens))
            start-symbol (second tokens)
            tokens (drop 2 tokens)

            rule-tokens (take (* 3 num-rules) tokens)
            tokens (drop (* 3 num-rules) tokens)

            prods (into [{:lhs \@ :rhs (str (first start-symbol))}]
                        (for [[lhs _arrow rhs] (partition 3 rule-tokens)]
                          {:lhs (first lhs)
                           :rhs (if (= rhs "#") "" rhs)}))

            num-prods (count prods)]

        (when (seq tokens)
          (let [num-queries (Long/parseLong (first tokens))
                queries (take num-queries (rest tokens))]
            (doseq [w queries]
              (let [w (if (= w "EPS") "" w)
                    n (count w)
                    chart (make-array java.util.ArrayList (inc n))
                    in-chart (make-array java.util.HashSet (inc n))]
                (dotimes [i (inc n)]
                  (aset chart i (java.util.ArrayList.))
                  (aset in-chart i (java.util.HashSet.)))

                (let [add-item (fn [idx item]
                                 (let [^java.util.HashSet s (aget in-chart idx)
                                       ^java.util.ArrayList c (aget chart idx)]
                                   (when (.add s item)
                                     (.add c item))))]
                  (add-item 0 [0 0 0])

                  (let [total-items
                        (loop [k 0
                               tot 0]
                          (if (> k n)
                            tot
                            (let [^java.util.ArrayList c-k (aget chart k)]
                              (loop [i 0]
                                (when (< i (.size c-k))
                                  (let [[rule-id dot origin] (.get c-k i)
                                        prod (nth prods rule-id)
                                        rhs (:rhs prod)
                                        rhs-len (count rhs)]
                                    (if (< dot rhs-len)
                                      (let [next-sym (nth rhs dot)]
                                        (if-not (terminal-symbol? next-sym)
                                          ;; Predictor
                                          (dotimes [r num-prods]
                                            (when (= (:lhs (nth prods r)) next-sym)
                                              (add-item k [r 0 k])))
                                          ;; Scanner
                                          (when (and (< k n) (= (nth w k) next-sym))
                                            (add-item (inc k) [rule-id (inc dot) origin]))))
                                      ;; Completer
                                      (let [completed-lhs (:lhs prod)
                                            ^java.util.ArrayList origin-c (aget chart origin)
                                            origin-len (.size origin-c)]
                                        (dotimes [j origin-len]
                                          (let [[p-rule-id p-dot p-origin] (.get origin-c j)
                                                prev-prod (nth prods p-rule-id)
                                                prev-rhs (:rhs prev-prod)]
                                            (when (and (< p-dot (count prev-rhs))
                                                       (= (nth prev-rhs p-dot) completed-lhs))
                                              (add-item k [p-rule-id (inc p-dot) p-origin])))))))
                                  (recur (inc i))))
                              (recur (inc k) (+ tot (.size c-k))))))]

                    (let [^java.util.ArrayList c-n (aget chart n)
                          accepted (loop [idx 0]
                                     (if (>= idx (.size c-n))
                                       false
                                       (let [[rule-id dot origin] (.get c-n idx)]
                                         (if (and (zero? rule-id) (= dot 1) (zero? origin))
                                           true
                                           (recur (inc idx))))))]
                      (if accepted
                        (println (str "ACCEPT " total-items))
                        (println (str "REJECT " total-items))))))))))))))

(-main)
