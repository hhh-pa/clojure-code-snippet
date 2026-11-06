(require '[clojure.string :as str])
;; mod_linear_solver: modular linear equation ax≡b mod n (CLRS 31.4). All solutions.
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
    (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))
(defn -main [input]
  (let [toks (tokenize input)
        a (Long/parseLong (nth toks 0)) b (Long/parseLong (nth toks 1)) n (Long/parseLong (nth toks 2))]
    (cond (= n 1) (println "0")
          (<= n 0) (println "NONE")
          :else (let [a (mod a n) b (mod b n)
                      [g x _] (egcd a n)]
                  (if (not= 0 (mod b g)) (println "NONE")
                    (let [n2 (quot n g) x0 (mod (* x (quot b g)) n)
                          sols (sort (map #(mod (+ x0 (* % n2)) n) (range g)))]
                      (println g) (println (str/join " " sols))))))))
(-main (slurp *in*))