;; Wildcard matching with ? and * (O(n*m) DP, full match).
(require '[clojure.string :as str])

(defn- wc-match?
  [text pat]
  (let [n (count text) m (count pat)
        init (loop [j 1 dp (assoc (vec (repeat (inc m) false)) 0 true)]
               (if (> j m)
                 dp
                 (recur (inc j)
                        (assoc dp j (and (nth dp (dec j)) (= (nth pat (dec j)) \*))))))]
    (loop [i 1 dp init]
      (if (> i n)
        (nth dp m)
        (recur (inc i)
               (loop [j 1 ndp (vec (repeat (inc m) false))]
                 (if (> j m)
                   ndp
                   (let [c (nth pat (dec j))]
                     (recur (inc j)
                            (assoc ndp j
                                   (cond
                                     (= c \*) (or (nth ndp (dec j)) (nth dp j))
                                     (or (= c \?) (= c (nth text (dec i)))) (nth dp (dec j))
                                     :else false)))))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [parts (str/split s #"\n")
          q (Long/parseLong (str/trim (first parts)))
          lines (vec (take (* 2 q) (concat (rest parts) (repeat ""))))]
      (loop [i 0 out []]
        (if (>= i q)
          (print (str/join "\n" out))
          (let [text (str/replace (nth lines (* 2 i)) #"\r$" "")
                pat (str/replace (nth lines (inc (* 2 i))) #"\r$" "")]
            (recur (inc i) (conj out (if (wc-match? text pat) "YES" "NO")))))))))
