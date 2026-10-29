(ns compiler.pratt-parser
  (:require [clojure.string :as str]))

(defn tokenize [^String s]
  (let [n (count s)]
    (loop [i 0
           tokens []]
      (if (>= i n)
        tokens
        (let [c (.charAt s i)]
          (cond
            (Character/isWhitespace c)
            (recur (inc i) tokens)

            (Character/isDigit c)
            (let [j (loop [k i]
                      (if (and (< k n) (Character/isDigit (.charAt s k)))
                        (recur (inc k))
                        k))
                  val (Long/parseLong (subs s i j))]
              (recur j (conj tokens [:num val])))

            (= c \+) (recur (inc i) (conj tokens [:+ nil]))
            (= c \-) (recur (inc i) (conj tokens [:- nil]))
            (= c \*) (recur (inc i) (conj tokens [:* nil]))
            (= c \/) (recur (inc i) (conj tokens [:/ nil]))
            (= c \^) (recur (inc i) (conj tokens [:pow nil]))
            (= c \() (recur (inc i) (conj tokens [:lparen nil]))
            (= c \)) (recur (inc i) (conj tokens [:rparen nil]))
            :else (recur (inc i) tokens)))))))

(defn infix-binding-power [token-type]
  (case token-type
    (:+ :-) [10 11]
    (:* :/) [20 21]
    :pow [31 30]
    [-1 -1]))

(defn to-sexpr [node]
  (case (first node)
    :num (str (second node))
    :unary (let [op (nth node 1)] (str "(" (if (= op :pow) "^" (name op)) " " (to-sexpr (nth node 2)) ")"))
    :binary (let [op (nth node 1)] (str "(" (if (= op :pow) "^" (name op)) " " (to-sexpr (nth node 2)) " " (to-sexpr (nth node 3)) ")"))))

(defn eval-ast [node]
  (case (first node)
    :num (second node)
    :unary (let [op (nth node 1)
                 child (eval-ast (nth node 2))]
             (if (= op :-) (- child) child))
    :binary (let [op (nth node 1)
                  l (eval-ast (nth node 2))
                  r (eval-ast (nth node 3))]
              (case op
                :+ (+ l r)
                :- (- l r)
                :* (* l r)
                :/ (if (zero? r) 0 (quot l r))
                :pow (reduce * 1 (repeat r l))))))

(defn parse-expr [tokens pos min-bp]
  (let [token (get tokens pos)]
    (when token
      (let [[tt val] token
            prefix-res
            (case tt
              :num [(inc pos) [:num val]]
              :- (let [rhs-res (parse-expr tokens (inc pos) 40)]
                   (when rhs-res
                     (let [[new-pos rhs] rhs-res]
                       [new-pos [:unary :- rhs]])))
              :lparen (let [inner-res (parse-expr tokens (inc pos) 0)]
                        (when inner-res
                          (let [[new-pos inner] inner-res]
                            (if (= (first (get tokens new-pos)) :rparen)
                              [(inc new-pos) inner]
                              [new-pos inner]))))
              nil)]
        (when prefix-res
          (loop [cur-pos (first prefix-res)
                 lhs (second prefix-res)]
            (let [next-token (get tokens cur-pos)]
              (if-not next-token
                [cur-pos lhs]
                (let [op-type (first next-token)
                      [l-bp r-bp] (infix-binding-power op-type)]
                  (if (< l-bp min-bp)
                    [cur-pos lhs]
                    (let [rhs-res (parse-expr tokens (inc cur-pos) r-bp)]
                      (if-not rhs-res
                        nil
                        (let [[new-pos rhs] rhs-res]
                          (recur new-pos [:binary op-type lhs rhs]))))))))))))))

(defn -main [& _args]
  (let [lines (line-seq (java.io.BufferedReader. *in*))]
    (when (seq lines)
      (let [lines (remove str/blank? lines)
            num-exprs (Long/parseLong (str/trim (first lines)))
            expr-lines (take num-exprs (rest lines))]
        (doseq [line expr-lines]
          (let [tokens (tokenize line)
                parsed (parse-expr tokens 0 0)]
            (if parsed
              (let [ast (second parsed)]
                (println (str (to-sexpr ast) " = " (eval-ast ast))))
              (println "PARSE_ERROR"))))))))

(-main)
