;; Lottery scheduling (MOS 2.4, Waldspurger-Weihl).
(require '[clojure.string :as str])

(def ^:const MOD (bit-shift-left 1 31))

(defn- lotto-draw
  [tix total rng]
  (if (zero? total)
    [rng "EMPTY"]
    (let [lim (* (quot MOD total) total)
          rng (loop [r rng]
                (let [nr (mod (+ (* 1103515245 r) 12345) MOD)]
                  (if (< nr lim) nr (recur nr))))
          w (mod rng total)
          win (loop [p 0 acc 0]
                (let [na (+ acc (nth tix p))]
                  (if (< w na) p (recur (inc p) na))))]
      [rng (str "WINNER " win)])))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          n (Long/parseLong (next-tok))
          e (Long/parseLong (next-tok))
          seed (Long/parseLong (next-tok))]
      (loop [i 0 tix (vec (repeat n 0)) total 0 rng seed out []]
        (if (>= i e)
          (print (str/join "\n" out))
          (let [op (next-tok)]
            (cond
              (= op "DRAW") (let [[nr line] (lotto-draw tix total rng)]
                             (recur (inc i) tix total nr (conj out line)))
              (= op "SET") (let [p (Long/parseLong (next-tok))
                                 t (Long/parseLong (next-tok))]
                             (recur (inc i) (assoc tix p t) (+ total (- t (nth tix p)))
                                    rng (conj out (str "OK " (+ total (- t (nth tix p)))))))
              :else (let [a (Long/parseLong (next-tok))
                          b (Long/parseLong (next-tok))
                          t (Long/parseLong (next-tok))]
                      (if (< (nth tix a) t)
                        (recur (inc i) tix total rng (conj out "REJECTED"))
                        (let [ntix (if (= a b)
                                     tix
                                     (-> tix (assoc a (- (nth tix a) t))
                                          (assoc b (+ (nth tix b) t))))]
                          (recur (inc i) ntix total rng
                                 (conj out (str "OK " total)))))))))))))
