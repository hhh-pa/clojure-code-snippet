(require '[clojure.string :as str])

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- find-root! [parent x]
  (let [root (loop [curr x]
               (let [p (nth parent curr)]
                 (if (= p curr)
                   curr
                   (recur p))))]
    (loop [curr x]
      (if (= curr root)
        root
        (let [nxt (nth parent curr)]
          (assoc! parent curr root)
          (recur nxt))))))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [n (parse-long (nth toks 0))]
              (if (or (nil? n) (neg? n))
                (report "invalid input: expected a node count n")
                (let [parent (transient (vec (range n)))]
                  (loop [idx 1
                         comps n
                         out (transient [])]
                    (if (>= (+ idx 1) nt)
                      (let [res (persistent! out)]
                        (when (seq res)
                          (println (str/join "\n" res))))
                      (let [u (dec (parse-long (nth toks idx)))
                            v (dec (parse-long (nth toks (inc idx))))]
                        (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                          (report "invalid input: edge endpoints must sit within 1 and n")
                          (let [ru (find-root! parent u)
                                rv (find-root! parent v)]
                            (if (= ru rv)
                              (recur (+ idx 2) comps (conj! out (str comps " CYCLE FORMED!")))
                              (do
                                (assoc! parent rv ru)
                                (let [ncomps (dec comps)]
                                  (recur (+ idx 2) ncomps (conj! out (str ncomps))))))))))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
