;; Open-addressing hash table with double hashing (CLRS 11.4).
(require '[clojure.string :as str])

(defn- dh-mod [a m]
  (let [r (rem a m)]
    (if (neg? r) (+ r m) r)))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          m (nth toks 0)
          n (nth toks 1)
          keys (subvec toks 2 (+ 2 n))
          table (loop [ks keys t (vec (repeat m nil))]
                  (if (empty? ks)
                    t
                    (let [k (first ks)
                          h1 (dh-mod k m)
                          h2 (inc (dh-mod k (dec m)))
                          j (loop [i 0]
                              (let [p (dh-mod (+ h1 (* i h2)) m)]
                                (if (nil? (nth t p)) p (recur (inc i)))))]
                      (recur (rest ks) (assoc t j k)))))]
      (print (str/join " " (map #(if (nil? %) "_" (str %)) table))))))
