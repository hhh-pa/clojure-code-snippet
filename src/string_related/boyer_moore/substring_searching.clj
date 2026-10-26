;; Boyer-Moore (bad-character rule) substring search. Port of
;; cpp/src/string_related/boyer_moore/substring_searching.cc (+ Go mirror).
;; Input: n then n pairs of <text> <pattern> tokens. Output YES/NO per pair.
(require '[clojure.string :as str])

(defn- bm-contains? [^chars text ^chars pat]
  (let [n (alength text)
        m (alength pat)]
    (cond
      (zero? m) true
      (< n m) false
      :else
      (let [right (int-array 256 -1)]
        (dotimes [j m]
          (aset right (bit-and (int (aget pat j)) 255) j))
        (loop [i (int 0)]
          (if (> (+ i m) n)
            false
            (let [skip (loop [j (int (dec m))]
                         (if (neg? j)
                           (int 0)
                           (if (= (aget pat j) (aget text (+ i j)))
                             (recur (dec j))
                             (let [s (- j (aget right (bit-and (int (aget text (+ i j))) 255)))]
                               (int (if (< s 1) 1 s))))))]
              (if (zero? skip)
                true
                (recur (+ i skip))))))))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          t (Integer/parseInt (first toks))
          sb (StringBuilder.)]
      (loop [ps (take (* 2 t) (rest toks))]
        (when (seq ps)
          (let [^String text (first ps)
                ^String pat (second ps)]
            (.append sb (if (bm-contains? (.toCharArray text) (.toCharArray pat))
                          "YES\n" "NO\n"))
            (recur (drop 2 ps)))))
      (print (.toString sb)))))
