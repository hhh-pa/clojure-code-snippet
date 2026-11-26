;; Page-table walk address translation (CS:APP ch 9).
(require '[clojure.string :as str])

(defn- at-translate
  [w m k paths va]
  (if (>= va (bit-shift-left 1 (+ (* k w) m)))
    "FAULT 0"
    (let [vpn (bit-shift-right va m)
          mask (dec (bit-shift-left 1 w))
          idx (mapv (fn [i] (bit-and (bit-shift-right vpn (* w (- k 1 i))) mask))
                    (range k))
          fault (some (fn [lv]
                        (when-not (some (fn [[vpns _]] (= (subvec vpns 0 lv) (subvec idx 0 lv)))
                                        paths)
                          lv))
                      (range 1 (inc k)))]
      (if fault
        (str "FAULT " fault)
        (let [ppn (reduce (fn [acc [vpns p]] (if (= vpns idx) p acc)) -1 paths)]
          (if (neg? ppn)
            (str "FAULT " k)
            (str "PA " (bit-or (bit-shift-left ppn m)
                               (bit-and va (dec (bit-shift-left 1 m)))))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          next-int (fn [] (Long/parseLong (next-tok)))
          w (next-int) m (next-int) k (next-int)
          t (next-int)
          paths (vec (for [_ (range t)]
                       [(vec (for [_ (range k)] (next-int))) (next-int)]))
          q (next-int)
          queries (vec (for [_ (range q)] (next-int)))]
      (print (str/join "\n" (map #(at-translate w m k paths %) queries))))))
