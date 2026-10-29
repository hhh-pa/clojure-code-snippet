(defn is-alnum [^Character c]
  (Character/isLetterOrDigit c))

(defn get-prec [c]
  (case c
    \* 3
    \. 2
    \| 1
    0))

(defn insert-concat [^String regex]
  (let [n (.length regex)]
    (loop [i 0 sb (StringBuilder.)]
      (if (< i n)
        (let [c1 (.charAt regex i)]
          (.append sb c1)
          (when (< (inc i) n)
            (let [c2 (.charAt regex (inc i))
                  c1-can-end (or (is-alnum c1) (= c1 \*) (= (int c1) 41))
                  c2-can-start (or (is-alnum c2) (= (int c2) 40))]
              (when (and c1-can-end c2-can-start)
                (.append sb \.))))
          (recur (inc i) sb))
        (.toString sb)))))

(defn to-postfix [^String infix]
  (let [sb (StringBuilder.)
        stk (java.util.ArrayDeque.)]
    (doseq [i (range (.length infix))
            :let [c (.charAt infix i)]]
      (cond
        (is-alnum c) (.append sb c)
        (= (int c) 40) (.push stk c)
        (= (int c) 41) (do
                         (while (and (not (.isEmpty stk)) (not= (int (.peek stk)) 40))
                           (.append sb (.pop stk)))
                         (when (not (.isEmpty stk)) (.pop stk)))
        :else (do
                (while (and (not (.isEmpty stk))
                            (not= (int (.peek stk)) 40)
                            (>= (get-prec (.peek stk)) (get-prec c)))
                  (.append sb (.pop stk)))
                (.push stk c))))
    (while (not (.isEmpty stk))
      (.append sb (.pop stk)))
    (.toString sb)))

(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          pattern (.next toks)
          num-cases (if (.hasNextInt toks) (.nextInt toks) 0)
          cases (vec (repeatedly num-cases #(.next toks)))
          infix (insert-concat pattern)
          postfix (to-postfix infix)
          ;; Build NFA
          trans (java.util.ArrayList.)
          add-state (fn []
                      (.add trans (java.util.ArrayList.))
                      (dec (.size trans)))
          add-trans (fn [from to ch]
                      (.add ^java.util.ArrayList (.get trans (int from)) [ch to]))
          stk (java.util.ArrayDeque.)]
      (doseq [i (range (.length postfix))
              :let [c (.charAt postfix i)]]
        (cond
          (= c \.) (let [f2 (.pop stk)
                         f1 (.pop stk)]
                     (add-trans (nth f1 1) (nth f2 0) \u0000)
                     (.push stk [(nth f1 0) (nth f2 1)]))
          (= c \|) (let [f2 (.pop stk)
                         f1 (.pop stk)
                         s (add-state)
                         e (add-state)]
                     (add-trans s (nth f1 0) \u0000)
                     (add-trans s (nth f2 0) \u0000)
                     (add-trans (nth f1 1) e \u0000)
                     (add-trans (nth f2 1) e \u0000)
                     (.push stk [s e]))
          (= c \*) (let [f (.pop stk)
                         s (add-state)
                         e (add-state)]
                     (add-trans s (nth f 0) \u0000)
                     (add-trans s e \u0000)
                     (add-trans (nth f 1) (nth f 0) \u0000)
                     (add-trans (nth f 1) e \u0000)
                     (.push stk [s e]))
          :else (let [s (add-state)
                      e (add-state)]
                  (add-trans s e c)
                  (.push stk [s e]))))
      (let [frag (.pop stk)
            start-state (nth frag 0)
            end-state (nth frag 1)
            get-closure (fn [start-set]
                          (let [closure (java.util.HashSet. ^java.util.Collection start-set)
                                q (java.util.ArrayDeque. ^java.util.Collection start-set)]
                            (while (not (.isEmpty q))
                              (let [u (int (.pop q))]
                                (doseq [[ch v] (.get trans u)]
                                  (when (= ch \u0000)
                                    (when (.add closure (int v))
                                      (.push q (int v)))))))
                            closure))]
        (doseq [tc cases]
          (let [res (loop [i 0
                           curr (get-closure [start-state])]
                      (if (< i (.length ^String tc))
                        (let [ch (.charAt ^String tc i)
                              next-states (java.util.HashSet.)]
                          (doseq [u curr]
                            (doseq [[c v] (.get trans (int u))]
                              (when (= c ch)
                                (.add next-states (int v)))))
                          (let [closed (get-closure next-states)]
                            (if (.isEmpty closed)
                              false
                              (recur (inc i) closed))))
                        (.contains curr (int end-state))))]
            (if res
              (println "MATCH")
              (println "NO_MATCH"))))))))
