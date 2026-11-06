;; second-best-mst: MST and second-best spanning tree
;;
;; Kruskal MST, then the best single-exchange tree over non-tree
;; edges using the max tree-path edge. Emits MST weight and the
;; second-best weight.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          es (mapv (fn [i] [(nth nums (+ 2 (* i 3))) (nth nums (+ 3 (* i 3))) (nth nums (+ 4 (* i 3)))]) (range m))
          o (vec (sort-by (fn [i] [(nth (nth es i) 2) (nth (nth es i) 0) (nth (nth es i) 1)]) (range m)))
          p (atom (vec (range n))) r (atom (vec (repeat n 0)))
          find (fn [x] (loop [x x] (if (= (nth @p x) x) x (recur (nth @p x)))))
          uni (fn [a b]
                (let [ra (find a) rb (find b)]
                  (if (= ra rb) false
                    (let [[ra rb] (if (< (nth @r ra) (nth @r rb)) [rb ra] [ra rb])]
                      (swap! p assoc rb ra)
                      (when (= (nth @r ra) (nth @r rb)) (swap! r assoc ra (inc (nth @r ra))))
                      true))))
          used (atom (vec (repeat m false)))
          mw (atom 0) cnt (atom 0)]
      (doseq [i o]
        (let [[u v w] (nth es i)]
          (when (uni u v)
            (swap! used assoc i true)
            (swap! mw + w)
            (swap! cnt inc))))
      (if (not= @cnt (dec n))
        (println "DISCONNECTED")
        (let [ta (reduce (fn [acc i] (if (nth @used i)
                                      (let [[u v w] (nth es i)]
                                        (-> acc (update u conj [v w]) (update v conj [u w])))
                                      acc))
                         (mapv (fn [_] []) (range n)) (range m))
              maxe (fn [s t]
                     (if (= s t) 0
                       (let [par (atom (assoc (vec (repeat n -1)) s s))
                             pw (atom (vec (repeat n 0)))]
                         (loop [q (conj clojure.lang.PersistentQueue/EMPTY s)]
                           (when (seq q)
                             (let [u (peek q) rst (pop q)]
                               (if (= u t)
                                 (recur rst)
                                 (let [nxt (atom rst)]
                                   (doseq [[w c] (nth ta u)]
                                     (when (= (nth @par w) -1)
                                       (swap! par assoc w u)
                                       (swap! pw assoc w c)
                                       (swap! nxt conj w)))
                                   (recur @nxt))))))
                         (loop [x t mx 0]
                           (if (= x s) mx (recur (nth @par x) (max mx (nth @pw x))))))))
              best (atom nil)]
          (doseq [i (range m)]
            (when (not (nth @used i))
              (let [[u v w] (nth es i)
                    c (+ @mw w (- (maxe u v)))]
                (when (or (nil? @best) (< c @best))
                  (reset! best c)))))
          (println @mw)
          (println (if (nil? @best) "NO SECOND" @best)))))))
