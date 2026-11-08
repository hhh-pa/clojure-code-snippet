(require '[clojure.string :as str])

;; Optimal bounds: the tree is built in O(n), then each range minimum query
;; and point update costs O(log n) in O(n) space, the classic segment bound.
;; Mirrors python-code-snippet/src/data_structure/segment_tree/range_minimum_query.py.

(def ^:const no-overlap 2147483647)

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- report [msg]
  (binding [*out* *err*]
    (println msg)))

(defn- range-min [tree size ql qr]
  (loop [l (+ ql size)
         r (+ qr size 1)
         res no-overlap]
    (if (>= l r)
      res
      (let [res1 (if (odd? l)
                   (min res (nth tree l))
                   res)
            l1 (if (odd? l) (inc l) l)
            r1 (if (odd? r) (dec r) r)
            res2 (if (odd? r)
                   (min res1 (nth tree r1))
                   res1)]
        (recur (bit-shift-right l1 1)
               (bit-shift-right r1 1)
               res2)))))

(defn- point-update! [tree size idx val]
  (let [pos (+ idx size)]
    (assoc! tree pos val)
    (loop [p (bit-shift-right pos 1)]
      (if (<= p 0)
        tree
        (let [l (bit-shift-left p 1)
              r (inc l)
              lv (nth tree l)
              rv (nth tree r)]
          (assoc! tree p (if (<= lv rv) lv rv))
          (recur (bit-shift-right p 1)))))))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [n (parse-long (nth toks 0))
                  q (parse-long (nth toks 1))]
              (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< nt (+ 2 n)))
                (report "invalid input: expected n and q, then n values, then q ops")
                (let [size (loop [s 1] (if (>= s n) s (recur (bit-shift-left s 1))))
                      tree (transient (vec (repeat (* 2 size) no-overlap)))]
                  (loop [i 0]
                    (when (< i n)
                      (assoc! tree (+ size i) (parse-long (nth toks (+ 2 i))))
                      (recur (inc i))))
                  (loop [i (dec size)]
                    (when (pos? i)
                      (let [l (bit-shift-left i 1)
                            r (inc l)
                            lv (nth tree l)
                            rv (nth tree r)]
                        (assoc! tree i (if (<= lv rv) lv rv))
                        (recur (dec i)))))
                  (let [op-base (+ 2 n)]
                    (loop [qi 0
                           idx op-base
                           buf (transient [])]
                      (if (>= qi q)
                        (let [res (persistent! buf)]
                          (when (seq res)
                            (print (str (str/join "\n" res) "\n"))))
                        (if (>= (+ idx 2) nt)
                          (let [res (persistent! buf)]
                            (when (seq res)
                              (print (str (str/join "\n" res) "\n"))))
                          (let [op (nth toks idx)
                                x (parse-long (nth toks (+ idx 1)))
                                y (parse-long (nth toks (+ idx 2)))]
                            (if (= op "q")
                              (let [ans (str (range-min tree size (dec x) (dec y)))
                                    nbuf (conj! buf ans)]
                                (if (>= (count nbuf) 4096)
                                  (do
                                    (print (str (str/join "\n" (persistent! nbuf)) "\n"))
                                    (recur (inc qi) (+ idx 3) (transient [])))
                                  (recur (inc qi) (+ idx 3) nbuf)))
                              (do
                                (point-update! tree size (dec x) y)
                                (recur (inc qi) (+ idx 3) buf)))))))))))))))
    (catch Exception _
      (report "invalid input"))))

(-main)
