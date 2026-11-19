;; RED gateway simulation (Tanenbaum 5.4 + Floyd-Jacobson 1993).
(require '[clojure.string :as str])

(def ^:const SCALE 1000)
(def ^:const MOD (bit-shift-left 1 31))

(defn- red-advance
  [{:keys [wqn wqd] :as st} t]
  (loop [q (:q st) avg (:avg st) now (:now st)]
    (if (> now t)
      (assoc st :q q :avg avg :now now)
      (if (pos? q)
        (recur (dec q) avg (inc now))
        (recur q (quot (* avg (- wqd wqn)) wqd) (inc now))))))

(defn- red-arrive
  [{:keys [wqn wqd mn mx pn pd cap q avg count rng] :as st}]
  (let [avg (quot (+ (* avg (- wqd wqn)) (* wqn q SCALE)) wqd)]
    (cond
      (= q cap) [(assoc st :avg avg :count 0) "DROP"]
      (< avg (* mn SCALE)) [(assoc st :avg avg :q (inc q) :count 0) "ENQUEUE"]
      (>= avg (* mx SCALE)) [(assoc st :avg avg :count 0) "DROP"]
      :else (let [count (inc count)
                  a (- avg (* mn SCALE))
                  r (* (- mx mn) SCALE)]
              (if (>= (* count pn a) (* pd r))
                [(assoc st :avg avg :count 0) "DROP"]
                (let [rng (mod (+ (* 1103515245 rng) 12345) MOD)]
                  (if (< (* rng (- (* pd r) (* count pn a))) (* MOD pn a))
                    [(assoc st :avg avg :count 0 :rng rng) "DROP"]
                    [(assoc st :avg avg :q (inc q) :count count :rng rng)
                     "ENQUEUE"])))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [nums (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          [wqn wqd mn mx pn pd cap seed] (take 8 nums)
          m (nth nums 8)
          arrivals (take m (drop 9 nums))
          st {:wqn wqn :wqd wqd :mn mn :mx mx :pn pn :pd pd :cap cap
              :q 0 :avg 0 :count 0 :rng seed :now 0}
          [fin out] (reduce (fn [[st o] t]
                              (let [[st2 line] (red-arrive (red-advance st t))]
                                [st2 (conj o line)]))
                            [st []] arrivals)]
      (print (str/join "\n" (conj out (str "FINAL " (:q fin) " "
                                           (:avg fin) " " (:count fin))))))))
