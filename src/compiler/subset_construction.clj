(ns compiler.subset-construction
  (:require [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn epsilon-closure [states nfa-trans]
  (loop [closure (into (sorted-set) states)
         q (vec states)]
    (if (empty? q)
      closure
      (let [u (first q)
            q (subvec q 1)
            trans (get nfa-trans u [])
            eps-tos (for [[c to] trans :when (= c \#) :when (not (contains? closure to))] to)
            new-closure (into closure eps-tos)
            new-q (into q eps-tos)]
        (recur new-closure new-q)))))

(defn move-states [states c nfa-trans]
  (into (sorted-set)
        (for [s states
              [ch to] (get nfa-trans s [])
              :when (= ch c)]
          to)))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [n-states (Long/parseLong (first tokens))
            start-state (Long/parseLong (second tokens))
            num-accept (Long/parseLong (nth tokens 2))
            accept-tokens (take num-accept (drop 3 tokens))
            nfa-accept (set (map #(Long/parseLong %) accept-tokens))
            tokens (drop (+ 3 num-accept) tokens)

            num-transitions (Long/parseLong (first tokens))
            tokens (rest tokens)

            trans-entries (take (* 3 num-transitions) tokens)
            tokens (drop (* 3 num-transitions) tokens)

            nfa-trans (reduce
                       (fn [m [u v c]]
                         (let [u (Long/parseLong u)
                               v (Long/parseLong v)
                               ch (first c)]
                           (update m u (fnil conj []) [ch v])))
                       {}
                       (partition 3 trans-entries))

            alphabet (into (sorted-set)
                           (for [[_ _ c] (partition 3 trans-entries)
                                 :let [ch (first c)]
                                 :when (not= ch \#)]
                             ch))

            start-set (epsilon-closure [start-state] nfa-trans)]

        ;; Subset construction
        (loop [work-queue [0]
               dfa-state-map {start-set 0}
               dfa-states [start-set]
               dfa-accept [(boolean (some nfa-accept start-set))]
               dfa-trans {0 {}}]
          (if (empty? work-queue)
            (do
              (println (str "DFA states: " (count dfa-states)))
              (println (str "DFA accept states: " (count (filter identity dfa-accept))))

              (when (seq tokens)
                (let [num-queries (Long/parseLong (first tokens))
                      queries (take num-queries (rest tokens))]
                  (doseq [w queries]
                    (let [w (if (= w "EPS") "" w)]
                      (loop [state 0
                             chars (seq w)]
                        (if (nil? chars)
                          (if (nth dfa-accept state)
                            (println "ACCEPT")
                            (println "REJECT"))
                          (let [ch (first chars)
                                nxt (get (get dfa-trans state) ch)]
                            (if (nil? nxt)
                              (println "REJECT")
                              (recur nxt (next chars)))))))))))

            (let [curr-id (first work-queue)
                  work-queue (subvec work-queue 1)
                  curr-set (nth dfa-states curr-id)

                  [next-wq next-map next-states next-accept next-trans]
                  (reduce
                   (fn [[wq s-map states accept-vec trans-map] c]
                     (let [moved (move-states curr-set c nfa-trans)
                           nxt (epsilon-closure moved nfa-trans)]
                       (if (empty? nxt)
                         [wq s-map states accept-vec trans-map]
                         (if-let [existing-id (get s-map nxt)]
                           [wq s-map states accept-vec (assoc-in trans-map [curr-id c] existing-id)]
                           (let [nxt-id (count states)
                                 is-acc (boolean (some nfa-accept nxt))]
                             [(conj wq nxt-id)
                              (assoc s-map nxt nxt-id)
                              (conj states nxt)
                              (conj accept-vec is-acc)
                              (assoc-in trans-map [curr-id c] nxt-id)])))))
                   [work-queue dfa-state-map dfa-states dfa-accept dfa-trans]
                   alphabet)]
              (recur next-wq next-map next-states next-accept next-trans))))))))

(-main)
