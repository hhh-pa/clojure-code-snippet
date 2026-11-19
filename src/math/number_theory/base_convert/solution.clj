;; base conversion on digit arrays: repeated divmod by the target
;; base, collecting remainders (no native bigint).
(require '[clojure.string :as str])

(def digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn base-convert [sb db dg]
  (let [cur (mapv #(.indexOf digits (str %)) (.toUpperCase dg))]
    (if (every? zero? cur)
      "0"
      (loop [cur cur
             out ()]
        (if (empty? cur)
          (apply str out)
          (let [[nxt carry] (loop [ds cur
                                   nxt []
                                   carry 0]
                              (if (empty? ds)
                                [nxt carry]
                                (let [v (+ (* carry sb) (first ds))
                                      q (quot v db)]
                                  (recur (rest ds)
                                         (if (or (seq nxt) (pos? q)) (conj nxt q) nxt)
                                         (rem v db)))))]
            (recur nxt (conj out (nth digits carry)))))))))

(defn -main
  "Read src-base, dst-base and digits; print the converted digits."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 3)
      (print (base-convert (Long/parseLong (nth toks 0))
                           (Long/parseLong (nth toks 1))
                           (nth toks 2))))))

(-main (slurp *in*))
