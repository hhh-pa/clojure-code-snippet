;; extended_euclid: extended GCD — g,x,y with ax+by=g (CLRS 31.2). Recursive. O(log min(a,b)).
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

(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))]
      [g y1 (- x1 (* (quot a b) y1))])))
(defn -main [input]
  (let [toks (tokenize input)
        a (Long/parseLong (nth toks 0)) b (Long/parseLong (nth toks 1))]
    (if (and (zero? a) (zero? b)) (println "0 0 0")
      (let [aa (Math/abs a) bb (Math/abs b)
            [g x y] (egcd aa bb)
            x (if (neg? a) (- x) x) y (if (neg? b) (- y) y)]
        (println g x y)))))
(-main (slurp *in*))

