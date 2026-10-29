(ns compiler.dfa-minimization
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [n (Long/parseLong (first tokens))
            start-state (Long/parseLong (second tokens))
            num-accept (Long/parseLong (nth tokens 2))
            accept-tokens (take num-accept (drop 3 tokens))
            accept-states (set (map #(Long/parseLong %) accept-tokens))
            tokens (drop (+ 3 num-accept) tokens)

            alpha-size (Long/parseLong (first tokens))
            alphabet (vec (map #(first %) (take alpha-size (rest tokens))))
            tokens (drop (inc alpha-size) tokens)

            num-trans (Long/parseLong (first tokens))
            tokens (rest tokens)

            trans-entries (take (* 3 num-trans) tokens)
            tokens (drop (* 3 num-trans) tokens)

            trans (make-array java.util.Map n)
            rev-trans (make-array java.util.Map n)]
        (dotimes [i n]
          (aset trans i (java.util.HashMap.))
          (aset rev-trans i (java.util.HashMap.)))

        (doseq [[u v c] (partition 3 trans-entries)]
          (let [u (Long/parseLong u)
                v (Long/parseLong v)
                ch (first c)]
            (.put ^java.util.Map (aget trans u) ch (int v))
            (let [^java.util.Map m (aget rev-trans v)
                  cur (.getOrDefault m ch [])]
              (.put m ch (conj cur (int u))))))

        ;; Reachability from start-state
        (let [reachable (boolean-array n)
              q (java.util.ArrayDeque.)]
          (aset reachable (int start-state) true)
          (.add q (int start-state))
          (while (not (.isEmpty q))
            (let [u (.poll q)
                  ^java.util.Map m (aget trans u)]
              (doseq [v (.values m)]
                (let [vi (int v)]
                  (when-not (aget reachable vi)
                    (aset reachable vi true)
                    (.add q vi))))))

          (let [states (filter #(aget reachable %) (range n))
                F (into (sorted-set) (filter accept-states states))
                nonF (into (sorted-set) (remove accept-states states))
                P-init (cond-> []
                         (seq F) (conj F)
                         (seq nonF) (conj nonF))
                W-init (cond
                         (and (seq F) (seq nonF))
                         (if (<= (count F) (count nonF)) [F] [nonF])
                         (seq F) [F]
                         (seq nonF) [nonF]
                         :else [])

                ;; Refinement loop
                [P _]
                (loop [P P-init
                       W (into clojure.lang.PersistentQueue/EMPTY W-init)]
                  (if (empty? W)
                    [P W]
                    (let [A (peek W)
                          W (pop W)
                          [P-new W-new]
                          (reduce
                           (fn [[p-acc w-acc] c]
                             (let [X (into (sorted-set)
                                           (for [a A
                                                 :let [^java.util.Map m (aget rev-trans a)
                                                       us (.get m c)]
                                                 :when us
                                                 u us
                                                 :when (aget reachable (int u))]
                                             u))]
                               (if (empty? X)
                                 [p-acc w-acc]
                                 (reduce
                                  (fn [[p-inner w-inner] Y]
                                    (let [inter (set/intersection Y X)
                                          diff (set/difference Y X)]
                                      (if (and (seq inter) (seq diff))
                                        (let [w-list (filter #(not= % Y) (seq w-inner))
                                              in-w (not= (count w-list) (count w-inner))
                                              w-next (into clojure.lang.PersistentQueue/EMPTY w-list)
                                              w-next (if in-w
                                                       (conj w-next inter diff)
                                                       (if (<= (count inter) (count diff))
                                                         (conj w-next inter)
                                                         (conj w-next diff)))]
                                          [(conj p-inner inter diff) w-next])
                                        [(conj p-inner Y) w-inner])))
                                  [[] w-acc]
                                  p-acc))))
                           [P W]
                           alphabet)]
                      (recur P-new W-new))))]

            ;; State to partition map
            (let [state-to-part (int-array n -1)]
              (doseq [[pid part] (map-indexed vector P)
                      s part]
                (aset state-to-part (int s) (int pid)))

              (let [min-start (aget state-to-part (int start-state))
                    min-accept (into (sorted-set)
                                     (keep-indexed
                                      (fn [pid part]
                                        (when (some accept-states part) pid))
                                      P))]
                (println (str "Minimal DFA states: " (count P)))
                (println (str "Accept states: " (count min-accept)))

                ;; Build minimized transitions
                (let [min-trans (vec (for [part P
                                           :let [rep (first part)
                                                 ^java.util.Map m (aget trans rep)]]
                                       (into {}
                                             (for [[c v] m
                                                   :when (aget reachable (int v))]
                                               [c (aget state-to-part (int v))]))))]

                  (when (seq tokens)
                    (let [num-queries (Long/parseLong (first tokens))
                          queries (take num-queries (rest tokens))]
                      (doseq [w queries]
                        (let [w (if (= w "EPS") "" w)]
                          (loop [curr min-start
                                 chars (seq w)]
                            (if (nil? chars)
                              (if (contains? min-accept curr)
                                (println "ACCEPT")
                                (println "REJECT"))
                              (let [ch (first chars)
                                    nxt (get (nth min-trans curr nil) ch)]
                                (if (nil? nxt)
                                  (println "REJECT")
                                  (recur nxt (next chars)))))))))))))))))))

(-main)
