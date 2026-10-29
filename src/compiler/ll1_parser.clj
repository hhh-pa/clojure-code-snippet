(ns compiler.ll1-parser
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn terminal? [c]
  (and (or (< (int c) (int \A)) (> (int c) (int \Z)))
       (not= c \#)))

(defn first-of-string [s first-sets]
  (if (or (= s "#") (empty? s))
    #{\#}
    (loop [chars (seq s)
           acc #{}]
      (if (nil? chars)
        (conj acc \#)
        (let [sym (first chars)
              sym-first (get first-sets sym #{})
              eps? (contains? sym-first \#)
              no-eps (disj sym-first \#)
              new-acc (into acc no-eps)]
          (if eps?
            (recur (next chars) new-acc)
            new-acc))))))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [num-prods (Long/parseLong (first tokens))
            start-symbol (first (second tokens))
            tokens (drop 2 tokens)

            prod-tokens (take (* 3 num-prods) tokens)
            tokens (drop (* 3 num-prods) tokens)

            prods (vec (for [[lhs _arrow rhs] (partition 3 prod-tokens)]
                         {:lhs (first lhs) :rhs rhs}))

            terminals (into #{\$}
                            (for [{:keys [rhs]} prods
                                  c rhs
                                  :when (terminal? c)]
                              c))

            ;; Step 1: FIRST sets
            first-init (into {} (for [t terminals] [t #{t}]))
            first-sets
            (loop [f first-init]
              (let [f-next
                    (reduce
                     (fn [acc {:keys [lhs rhs]}]
                       (let [old (get acc lhs #{})
                             add (if (= rhs "#")
                                   #{\#}
                                   (first-of-string rhs acc))]
                         (assoc acc lhs (into old add))))
                     f
                     prods)]
                (if (= f-next f)
                  f
                  (recur f-next))))

            ;; Step 2: FOLLOW sets
            follow-init {start-symbol #{\$}}
            follow-sets
            (loop [fol follow-init]
              (let [fol-next
                    (reduce
                     (fn [acc {:keys [lhs rhs]}]
                       (if (= rhs "#")
                         acc
                         (loop [i 0
                                cur-acc acc]
                           (if (>= i (count rhs))
                             cur-acc
                             (let [B (nth rhs i)]
                               (if (terminal? B)
                                 (recur (inc i) cur-acc)
                                 (let [rest-str (subs rhs (inc i))
                                       f-rest (first-of-string rest-str first-sets)
                                       no-eps (disj f-rest \#)
                                       cur-b (get cur-acc B #{})
                                       cur-b (into cur-b no-eps)
                                       cur-b (if (contains? f-rest \#)
                                               (into cur-b (get cur-acc lhs #{}))
                                               cur-b)]
                                   (recur (inc i) (assoc cur-acc B cur-b)))))))))
                     fol
                     prods)]
                (if (= fol-next fol)
                  fol
                  (recur fol-next))))

            ;; Step 3: LL(1) Table construction
            [table is-ll1]
            (reduce
             (fn [[t ok] [idx {:keys [lhs rhs]}]]
               (let [f-rhs (first-of-string rhs first-sets)
                     [t1 ok1]
                     (reduce
                      (fn [[tab status] a]
                        (if (= a \#)
                          [tab status]
                          (let [key [lhs a]]
                            (if (and (contains? tab key) (not= (get tab key) idx))
                              [(assoc tab key idx) false]
                              [(assoc tab key idx) status]))))
                      [t ok]
                      f-rhs)
                     [t2 ok2]
                     (if (contains? f-rhs \#)
                       (reduce
                        (fn [[tab status] b]
                          (let [key [lhs b]]
                            (if (and (contains? tab key) (not= (get tab key) idx))
                              [(assoc tab key idx) false]
                              [(assoc tab key idx) status])))
                        [t1 ok1]
                        (get follow-sets lhs #{}))
                       [t1 ok1])]
                 [t2 ok2]))
             [{} true]
             (map-indexed vector prods))]

        (if-not is-ll1
          (println "GRAMMAR IS NOT LL1")
          (do
            (println "GRAMMAR IS LL1")
            (when (seq tokens)
              (let [num-queries (Long/parseLong (first tokens))
                    queries (take num-queries (rest tokens))]
                (doseq [w queries]
                  (let [w (str w "$")
                        n (count w)]
                    (loop [stk [\$ start-symbol]
                           ip 0
                           derivations []]
                      (if (empty? stk)
                        (if (= ip n)
                          (println (str/join " " (map #(str "r" %) derivations)))
                          (println "SYNTAX ERROR"))
                        (let [X (peek stk)
                              stk (pop stk)
                              a (if (< ip n) (nth w ip) \$)]
                          (cond
                            (= X a)
                            (if (= X \$)
                              (if (= (inc ip) n)
                                (println (str/join " " (map #(str "r" %) derivations)))
                                (println "SYNTAX ERROR"))
                              (recur stk (inc ip) derivations))

                            (terminal? X)
                            (println "SYNTAX ERROR")

                            :else
                            (if-let [rule-idx (get table [X a])]
                              (let [rhs (:rhs (nth prods rule-idx))
                                    new-stk (if (= rhs "#")
                                              stk
                                              (into stk (reverse rhs)))]
                                (recur new-stk ip (conj derivations rule-idx)))
                              (println "SYNTAX ERROR"))))))))))))))))

(-main)
