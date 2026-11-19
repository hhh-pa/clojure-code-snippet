;; RAID-5 parity + single-disk reconstruction (MOS 5.4).
(require '[clojure.string :as str])

(defn- raid-write
  [{:keys [drives stripes data par failed] :as st} s d v]
  (if (or (neg? s) (>= s stripes) (neg? d) (>= d drives)
          (= d (mod s drives)) (contains? failed d))
    [st "REJECTED"]
    [(assoc st :par (assoc par s (bit-xor (nth par s) (nth (nth data s) d) v))
                :data (assoc data s (assoc (nth data s) d v)))
     "OK"]))

(defn- slot-val
  [data par s d pd]
  (if (= d pd) (nth par s) (nth (nth data s) d)))

(defn- raid-read
  [{:keys [drives stripes data par failed] :as st} s d]
  (cond
    (or (neg? s) (>= s stripes) (neg? d) (>= d drives)) [st "REJECTED"]
    (and (contains? failed d) (> (count failed) 1)) [st "UNRECOVERABLE"]
    (not (contains? failed d)) [st (str "BYTE " (slot-val data par s d (mod s drives)))]
    :else (let [pd (mod s drives)
                x (reduce bit-xor 0 (map #(slot-val data par s % pd)
                                          (remove #{d} (range drives))))]
            [st (str "BYTE " x)])))

(defn- raid-parity
  [{:keys [drives stripes data par failed] :as st} s]
  (if (or (neg? s) (>= s stripes))
    [st "REJECTED"]
    (let [pd (mod s drives)]
      (cond
        (and (contains? failed pd) (> (count failed) 1)) [st "UNRECOVERABLE"]
        (not (contains? failed pd)) [st (str "PARITY " pd " " (nth par s))]
        :else [st (str "PARITY " pd " "
                       (reduce bit-xor 0 (map #(nth (nth data s) %)
                                               (remove #{pd} (range drives)))))]))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          drives (Long/parseLong (next-tok))
          stripes (Long/parseLong (next-tok))
          e (Long/parseLong (next-tok))
          st {:drives drives :stripes stripes
              :data (vec (repeat stripes (vec (repeat drives 0))))
              :par (vec (repeat stripes 0)) :failed #{}}]
      (loop [i 0 st st out []]
        (if (>= i e)
          (print (str/join "\n" out))
          (let [op (next-tok)]
            (cond
              (= op "W") (let [s (Long/parseLong (next-tok))
                               d (Long/parseLong (next-tok))
                               v (Long/parseLong (next-tok))
                               [nst line] (raid-write st s d v)]
                           (recur (inc i) nst (conj out line)))
              (= op "R") (let [s (Long/parseLong (next-tok))
                               d (Long/parseLong (next-tok))
                               [nst line] (raid-read st s d)]
                           (recur (inc i) nst (conj out line)))
              (= op "F") (let [d (Long/parseLong (next-tok))]
                           (if (or (neg? d) (>= d drives))
                             (recur (inc i) st (conj out "REJECTED"))
                             (recur (inc i) (update st :failed conj d)
                                    (conj out "OK"))))
              :else (let [[nst line] (raid-parity st (Long/parseLong (next-tok)))]
                      (recur (inc i) nst (conj out line))))))))))
