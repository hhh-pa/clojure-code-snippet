;; Manacher longest palindromic substring + palindrome count. Port of
;; cpp/src/string_related/manacher/manacher.cc.
;; Input: single <string> token. Output max_len, longest palindrome,
;; total palindrome count, one per line.
(require '[clojure.string :as str])

(defn- manacher [^String s]
  (let [n (.length s)
        sb (StringBuilder. "^")]
    (loop [i (long 0)]
      (when (< i n)
        (.append sb \#)
        (.append sb (.charAt s i))
        (recur (inc i))))
    (.append sb "#$")
    (let [^chars tc (.toCharArray (.toString sb))
          m (alength tc)
          ^ints p (int-array m)]
      (loop [i (long 1) c (long 0) r (long 0)]
        (when (< i (dec m))
          (let [mirror (- (* 2 c) i)
                init (if (< i r) (min (- r i) (aget p mirror)) 0)
                rad (loop [rad (long init)]
                      (if (= (aget tc (+ i 1 rad)) (aget tc (- i 1 rad)))
                        (recur (inc rad))
                        rad))]
            (aset p i (int rad))
            (if (> (+ i rad) r)
              (recur (inc i) i (long (+ i rad)))
              (recur (inc i) c r)))))
      (let [[max-len center total]
            (loop [i (long 1) max-len (long 0) center (long 0) total (long 0)]
              (if (>= i (dec m))
                [max-len center total]
                (let [pi (aget p i)
                      [max-len center] (if (> pi max-len)
                                        [(long pi) i]
                                        [max-len center])]
                  (recur (inc i) max-len center
                         (long (+ total (quot (+ pi 1) 2)))))))]
        (let [start (quot (- center max-len) 2)]
          [max-len
           (.substring s (int start) (int (+ start max-len)))
           total])))))

(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [s (first (re-seq #"\S+" data))
          [max-len longest total] (manacher s)]
      (println max-len)
      (println longest)
      (println total))))
