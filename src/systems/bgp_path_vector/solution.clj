;; BGP path-vector routing simulation (Tanenbaum 5).
(require '[clojure.string :as str])

(defn- bgp-better?
  [cand-len frm cur cur-nh]
  (or (nil? cur)
      (< cand-len (count cur))
      (and (= cand-len (count cur)) (< frm cur-nh))))

(defn- bgp-round
  [n adj rib nh has disc]
  (let [adv (vec (for [a (range n) :when (nth has a) f (nth adj a)] [f a]))]
    (loop [ds adv rib rib nh nh has has disc disc changed false]
      (if (empty? ds)
        [rib nh has disc changed]
        (let [[to frm] (first ds)
              path (nth rib frm)]
          (if (some #{to} path)
            (recur (rest ds) rib nh has (update disc to inc) changed)
            (if (bgp-better? (inc (count path)) frm (nth rib to) (nth nh to))
              (recur (rest ds) (assoc rib to (vec (cons to path)))
                     (assoc nh to frm) (assoc has to true) disc true)
              (recur (rest ds) rib nh has disc changed))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          m (nth toks 1)
          adj (loop [i 0 a (vec (repeat n []))]
                (if (>= i m)
                  a
                  (let [u (nth toks (+ 2 (* 2 i))) v (nth toks (+ 3 (* 2 i)))]
                    (recur (inc i) (-> a (update u conj v) (update v conj u))))))
          origin (nth toks (+ 2 (* 2 m)))
          [rib nh has disc] (loop [r 0 rib (assoc (vec (repeat n nil)) origin [origin])
                                   nh (assoc (vec (repeat n -1)) origin origin)
                                   has (assoc (vec (repeat n false)) origin true)
                                   disc (vec (repeat n 0))]
                              (if (>= r (* n n))
                                [rib nh has disc]
                                (let [[rib nh has disc ch] (bgp-round n adj rib nh has disc)]
                                  (if ch
                                    (recur (inc r) rib nh has disc)
                                    [rib nh has disc]))))
          out (vec (for [a (range n)]
                     (cond
                       (= a origin) (str "ORIGIN " origin)
                       (not (nth has a)) "UNREACHABLE"
                       :else (str (nth nh a) " " (str/join " " (nth rib a))))))]
      (print (str/join "\n" (conj out (str "DISCARDS " (str/join " " disc))))))))
