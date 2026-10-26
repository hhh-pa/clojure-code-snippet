;; KMP (prefix-function) substring search. Port of
;; cpp/src/string_related/kmp/substring_searching.cc (+ Go mirror; the Go
;; prefix-function form is used instead of the 256xM DFA table, which is
;; behaviorally identical for YES/NO but uses O(M) memory).
;; Input: n then n pairs of <text> <pattern> tokens. Output YES/NO per pair.
(require '[clojure.string :as str])

(defn- kmp-contains? [^chars text ^chars pat]
  (let [m (alength pat)]
    (if (zero? m)
      true
      (let [n (alength text)
            pi (int-array m)]
        (loop [i (int 1) j (int 0)]
          (when (< i m)
            (let [j (loop [j j]
                      (if (and (pos? j) (not (= (aget pat i) (aget pat j))))
                        (recur (aget pi (dec j)))
                        j))
                  j (if (= (aget pat i) (aget pat j)) (inc j) j)]
              (aset pi i j)
              (recur (inc i) j))))
        (loop [i (int 0) j (int 0)]
          (if (>= i n)
            false
            (let [c (aget text i)
                  j (loop [j j]
                      (if (and (pos? j) (not (= c (aget pat j))))
                        (recur (aget pi (dec j)))
                        j))
                  j (if (= c (aget pat j)) (inc j) j)]
              (if (= j m)
                true
                (recur (inc i) j)))))))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (re-seq #"\S+" data)
          t (Integer/parseInt (first toks))
          sb (StringBuilder.)]
      (loop [ps (take (* 2 t) (rest toks))]
        (when (seq ps)
          (let [^String text (first ps)
                ^String pat (second ps)]
            (.append sb (if (kmp-contains? (.toCharArray text) (.toCharArray pat))
                          "YES\n" "NO\n"))
            (recur (drop 2 ps)))))
      (print (.toString sb)))))
