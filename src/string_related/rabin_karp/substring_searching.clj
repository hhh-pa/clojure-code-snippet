;; rabin-karp: rolling-hash substring search with verify-on-hit.
(require '[clojure.string :as str])

(def base 256)
(def modp 1000000007)

(defn rabin-karp [text pat]
  (let [n (count text)
        m (count pat)]
    (cond (zero? m) true
          (> m n) false
          :else (let [pw (reduce (fn [p _] (rem (* p base) modp)) 1 (range (dec m)))
                      h (fn [s] (reduce (fn [acc c] (rem (+ (* acc base) (int c)) modp)) 0 s))
                      ph (h (subs pat 0 m))
                      rolling (fn rolling [i th]
                                (cond (> i (- n m)) false
                                      (and (= ph th) (= (subs text i (+ i m)) pat)) true
                                      (= i (- n m)) false
                                      :else (let [out (int (nth text i))
                                                  in (int (nth text (+ i m)))
                                                  th2 (rem (+ (* (- th (rem (* out pw) modp)) base) in) modp)
                                                  th2 (if (neg? th2) (+ th2 modp) th2)]
                                              (recur (inc i) th2))))]
                  (rolling 0 (h (subs text 0 m)))))))

(defn -main
  "Read t then text/pattern line pairs; print YES/NO per pair."
  [input]
  (let [lines (str/split-lines input)]
    (when (seq lines)
      (let [t (Long/parseLong (str/trim (first lines)))
            pairs (take t (partition 2 2 [""] (rest lines)))]
        (print (str/join "\n" (map (fn [[text pat]] (if (rabin-karp (or text "") (or pat "")) "YES" "NO")) pairs)))))))

(-main (slurp *in*))
