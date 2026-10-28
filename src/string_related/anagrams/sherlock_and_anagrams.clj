(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          q (Integer/parseInt (aget ^objects toks 0))
          out (StringBuilder.)]
      (dotimes [qi q]
        (let [str-s ^String (aget ^objects toks (inc qi))
              n (.length str-s)
              counts (java.util.HashMap.)]
          (dotimes [len n]
            (let [l (inc len)]
              (dotimes [start (- (inc n) l)]
                (let [sub (subs str-s start (+ start l))
                      chars (.toCharArray ^String sub)]
                  (java.util.Arrays/sort chars)
                  (let [key (String. chars)
                        cnt (long (.getOrDefault counts key (long 0)))]
                    (.put counts key (inc cnt)))))))
          (let [ans (loop [entries (seq (.values counts)) tot (long 0)]
                      (if (empty? entries)
                        tot
                        (let [c (long (first entries))]
                          (recur (rest entries) (+ tot (quot (* c (dec c)) 2))))))]
            (.append out ans)
            (.append out \newline))))
      (print (str out)))))
