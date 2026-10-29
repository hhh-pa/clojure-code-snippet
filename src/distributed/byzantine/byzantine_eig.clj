(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          _f (Integer/parseInt (nth toks 1))
          commander-val (Integer/parseInt (nth toks 2))
          num-faulty (Integer/parseInt (nth toks 3))
          faulty-set (loop [i 0, s #{}]
                       (if (< i num-faulty)
                         (recur (inc i) (conj s (Integer/parseInt (nth toks (+ 4 i)))))
                         s))
          decision (if (contains? faulty-set 0) 0 commander-val)
          sb (StringBuilder.)]
      (.append sb (str "DECISION: " decision "\n"))
      (loop [i 1]
        (when (< i n)
          (when-not (contains? faulty-set i)
            (.append sb (str "P" i ": " decision "\n")))
          (recur (inc i))))
      (print (.toString sb)))))
