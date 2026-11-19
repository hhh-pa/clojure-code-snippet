;; Classical ciphers: Caesar and Vigenere enc/dec (case kept,
;; non-letters pass through) plus chi-squared Caesar breaking.
(require '[clojure.string :as str])

(def freq [8.2 1.5 2.8 4.3 13.0 2.2 2.0 6.1 7.0 0.15 0.77 4.0
           2.4 6.7 7.5 1.9 0.095 6.0 6.3 9.1 2.8 0.98 2.4 0.15 2.0 0.074])

(defn caesar [text shift]
  (apply str
         (map (fn [c]
                (cond (Character/isUpperCase ^char c)
                      (char (+ (int \A) (mod (+ (- (int c) (int \A)) shift) 26)))
                      (Character/isLowerCase ^char c)
                      (char (+ (int \a) (mod (+ (- (int c) (int \a)) shift) 26)))
                      :else c))
              text)))

(defn vigenere [text key decode]
  (let [ks (mapv #(mod (- (int (Character/toUpperCase ^char %)) (int \A)) 26)
                 (filter #(Character/isAlphabetic (int %)) key))]
    (loop [cs (seq text)
           j 0
           out []]
      (if (empty? cs)
        (apply str out)
        (let [c (first cs)]
          (if (not (Character/isAlphabetic (int c)))
            (recur (rest cs) j (conj out c))
            (let [s (nth ks (rem j (count ks)))
                  s (if decode (- s) s)
                  base (if (Character/isUpperCase ^char c) (int \A) (int \a))]
              (recur (rest cs) (inc j)
                     (conj out (char (+ base (mod (+ (- (int c) base) s) 26))))))))))))

(defn caesar-break [text]
  (let [letters (apply str (map #(Character/toUpperCase ^char %) (filter #(Character/isAlphabetic (int %)) text)))
        n (count letters)]
    (loop [shift 0
           best nil
           bests 0]
      (if (>= shift 26)
        (str bests " " (caesar text (- bests)))
        (let [pt (caesar letters (- shift))
              obs (mapv (fn [k] (count (filter #(= % (char (+ (int \A) k))) pt))) (range 26))
              chi (reduce + (map (fn [o f]
                                   (let [e (/ (* n f) 100.0)]
                                     (/ (* (- o e) (- o e)) e)))
                                 obs freq))]
          (if (or (nil? best) (< chi best))
            (recur (inc shift) chi shift)
            (recur (inc shift) best bests)))))))

(defn -main
  "Read OP and args; print the transformed text."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+" 2))]
    (when (>= (count toks) 1)
      (let [op (nth toks 0)
            rest (if (> (count toks) 1) (nth toks 1) "")]
        (cond (= op "BREAK") (print (caesar-break rest))
              :else
              (let [parts (remove str/blank? (str/split rest #"\s+" 2))
                    arg (nth parts 0)
                    text (if (> (count parts) 1) (nth parts 1) "")]
                (cond (= op "CAESAR_ENC") (print (caesar text (Long/parseLong arg)))
                      (= op "CAESAR_DEC") (print (caesar text (- (Long/parseLong arg))))
                      (= op "VIGENERE_ENC") (print (vigenere text arg false))
                      :else (print (vigenere text arg true)))))))))

(-main (slurp *in*))
