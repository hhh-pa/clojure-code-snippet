;; Sequencing by hybridization: Eulerian reconstruction (Hierholzer).

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+"))
      kmers (drop 2 toks)
      adj (reduce (fn [m km] (update m (subs km 0 (dec (count km))) (fnil conj []) km)) {} kmers)
      adj (into {} (map (fn [[u es]] [u (vec (reverse (sort es)))]) adj))
      deg (reduce (fn [m km]
                    (-> m
                        (update (subs km 0 (dec (count km))) (fnil + 0) 1)
                        (update (subs km 1) (fnil + 0) -1)))
                  {} kmers)
      nodes (sort (keys deg))
      start (or (first (filter #(= (get deg %) 1) nodes)) (first (sort (keys adj))))]
  (loop [stack [start] path [] g adj]
    (if (empty? stack)
      (let [p (reverse path)]
        (print (apply str (first p) (map #(nth % (dec (count %))) (rest p)))))
      (let [u (peek stack) es (get g u)]
        (if (seq es)
          (recur (conj stack (subs (peek es) 1)) path (update g u pop))
          (recur (pop stack) (conj path u) g))))))
