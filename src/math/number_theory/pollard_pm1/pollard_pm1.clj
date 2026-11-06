;; pollard_pm1: Pollard p-1 factorization. Smooth p-1 extraction.
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
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn gcd [a b] (if (zero? b) (Math/abs a) (recur b (mod a b))))
(defn -main [input]
  (let [toks (tokenize input)
        n (Long/parseLong (nth toks 0)) B (Long/parseLong (nth toks 1))]
    (if (<= n 1) (println "NONE")
      (loop [a 2 k 2]
        (if (> k B)
          (let [g (gcd (dec a) n)] (if (and (> g 1) (< g n)) (println g) (println "NONE")))
          (let [a2 (modpow a k n) g (gcd (dec a2) n)]
            (if (and (> g 1) (< g n)) (println g) (recur a2 (inc k)))))))))
(-main (slurp *in*))

