;; modexp: modular exponentiation b^e mod m. O(log e).
;; Pure Clojure with canonical input parsing.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn modpow [b e m]
  (if (= m 1) 0
    (loop [r (mod 1 m) b (mod b m) e e]
      (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2))))))
(defn -main [input]
  (let [toks (tokenize input)
        vals (mapv #(Long/parseLong %) toks)]
    (cond (= 3 (count vals)) (println (modpow (nth vals 0) (nth vals 1) (nth vals 2)))
          :else (let [t (first vals)]
                  (if (and (pos? t) (= (count vals) (inc (* 3 t))))
                    (doseq [i (range t)] (println (modpow (nth vals (+ 1 (* 3 i))) (nth vals (+ 2 (* 3 i))) (nth vals (+ 3 (* 3 i))))))
                    (println (modpow (nth vals 0) (nth vals 1) (nth vals 2))))))))
(-main (slurp *in*))

