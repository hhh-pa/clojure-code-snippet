;; Z-function substring search. Port of
;; cpp/src/string_related/z_function/substring_searching.cc (+ Go mirror).
;; Input: n then n pairs of <text> <pattern> tokens. Output YES/NO per pair.
(require '[clojure.string :as str])

(defn- z-contains? [^String text ^String pat]
  (let [m (.length pat)]
    (if (zero? m)
      true
      (let [^chars b (.toCharArray (.concat (.concat pat "$") text))
            n (alength b)
            z (int-array n)]
        (loop [i (int 1) l (int 0) r (int 0)]
          (if (>= i n)
            false
            (let [zi (if (<= i r) (min (- (inc r) i) (aget z (- i l))) (int 0))
                  zi (loop [zv zi]
                       (if (and (< (+ i zv) n) (= (aget b zv) (aget b (+ i zv))))
                         (recur (inc zv))
                         zv))]
              (aset z i zi)
              (if (= zi m)
                true
                (if (and (pos? (+ i zi)) (> (+ i zi -1) r))
                  (recur (inc i) i (+ i zi -1))
                  (recur (inc i) l r))))))))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          t (Integer/parseInt (first toks))
          sb (StringBuilder.)]
      (loop [ps (take (* 2 t) (rest toks))]
        (when (seq ps)
          (let [^String text (first ps)
                ^String pat (second ps)]
            (.append sb (if (z-contains? text pat) "YES\n" "NO\n"))
            (recur (drop 2 ps)))))
      (print (.toString sb)))))
