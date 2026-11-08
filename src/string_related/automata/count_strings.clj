(require '[clojure.string :as str])

(def ^:const MOD 1000000007)

(defn- tokenize [s]
  (if (str/blank? s)
    []
    (str/split (str/trim s) #"\s+")))

(defn- parse-regex [s]
  (let [n (count s)]
    (letfn [(peek-ch [i]
              (if (< i n) (nth s i) nil))
            (parse-expr [i]
              (let [[node i] (parse-term i)]
                (loop [node node i i]
                  (if (= (peek-ch i) \|)
                    (let [[rhs i] (parse-term (inc i))]
                      (recur {:kind :union :left node :right rhs} i))
                    [node i]))))
            (parse-term [i]
              (loop [parts (transient []) i i]
                (let [c (peek-ch i)]
                  (if (or (nil? c) (= c \)) (= c \|))
                    (let [p (persistent! parts)]
                      [(cond
                         (empty? p) {:kind :concat :kids []}
                         (= (count p) 1) (nth p 0)
                         :else {:kind :concat :kids p})
                       i])
                    (let [[f i] (parse-factor i)]
                      (recur (conj! parts f) i))))))
            (parse-factor [i]
              (let [[base i] (parse-base i)]
                (loop [base base i i]
                  (if (= (peek-ch i) \*)
                    (recur {:kind :star :left base} (inc i))
                    [base i]))))
            (parse-base [i]
              (let [c (peek-ch i)]
                (cond
                  (= c \()
                  (let [[node i] (parse-expr (inc i))]
                    [node (if (= (peek-ch i) \)) (inc i) i)])
                  (or (= c \a) (= c \b))
                  [{:kind :char :ch (if (= c \a) 1 2)} (inc i)]
                  :else
                  [{:kind :concat :kids []} i])))]
      (first (parse-expr 0)))))

(defn- build-nfa [ast adj]
  (let [start (count adj)
        _ (conj! adj [])
        end (count adj)
        _ (conj! adj [])]
    (case (:kind ast)
      :char
      (do (assoc! adj start (conj (nth adj start) [end (:ch ast)]))
          [start end])
      :concat
      (let [kids (:kids ast)]
        (if (empty? kids)
          (do (assoc! adj start (conj (nth adj start) [end 0]))
              [start end])
          (loop [ks kids prev -1]
            (if (empty? ks)
              (do (assoc! adj prev (conj (nth adj prev) [end 0]))
                  [start end])
              (let [[i o] (build-nfa (first ks) adj)]
                (if (= prev -1)
                  (assoc! adj start (conj (nth adj start) [i 0]))
                  (assoc! adj prev (conj (nth adj prev) [i 0])))
                (recur (rest ks) o))))))
      :union
      (let [[li lo] (build-nfa (:left ast) adj)
            [ri ro] (build-nfa (:right ast) adj)]
        (assoc! adj start (conj (nth adj start) [li 0] [ri 0]))
        (assoc! adj lo (conj (nth adj lo) [end 0]))
        (assoc! adj ro (conj (nth adj ro) [end 0]))
        [start end])
      :star
      (let [[ii io] (build-nfa (:left ast) adj)]
        (assoc! adj start (conj (nth adj start) [ii 0] [end 0]))
        (assoc! adj io (conj (nth adj io) [ii 0] [end 0]))
        [start end]))))

(defn- eclosure-set [adj num-states starts]
  (let [seen (transient (vec (repeat num-states false)))
        queue (transient [])]
    (doseq [s starts]
      (when-not (nth seen s)
        (assoc! seen s true)
        (conj! queue s)))
    (loop [head 0 qlen (count queue)]
      (when (< head qlen)
        (let [u (nth queue head)
              edges (nth adj u)
              nql (loop [k 0 ql qlen]
                    (if (= k (count edges))
                      ql
                      (let [[v lab] (nth edges k)]
                        (if (and (zero? lab) (not (nth seen v)))
                          (do (assoc! seen v true)
                              (conj! queue v)
                              (recur (inc k) (inc ql)))
                          (recur (inc k) ql)))))]
          (recur (inc head) nql))))
    (loop [i 0 acc (transient [])]
      (if (= i num-states)
        (persistent! acc)
        (if (nth seen i)
          (recur (inc i) (conj! acc i))
          (recur (inc i) acc))))))

(defn- move-set [adj states target-lab]
  (let [res (transient #{})]
    (doseq [u states]
      (doseq [[v lab] (nth adj u)]
        (when (= lab target-lab)
          (conj! res v))))
    (vec (sort (persistent! res)))))

(defn- mat-mul [a b n]
  (let [r (transient (vec (repeat (* n n) 0)))]
    (loop [i 0]
      (when (< i n)
        (let [i-off (* i n)]
          (loop [k 0]
            (when (< k n)
              (let [aik (nth a (+ i-off k))]
                (when (pos? aik)
                  (let [k-off (* k n)]
                    (loop [j 0]
                      (when (< j n)
                        (let [bkj (nth b (+ k-off j))]
                          (when (pos? bkj)
                            (let [idx (+ i-off j)
                                  cur (nth r idx)]
                              (assoc! r idx (rem (+ cur (* aik bkj)) MOD))))
                          (recur (inc j))))))))
              (recur (inc k))))
          (recur (inc i)))))
    (persistent! r)))

(defn- mat-pow [m l n]
  (let [ident (transient (vec (repeat (* n n) 0)))]
    (loop [i 0]
      (when (< i n)
        (assoc! ident (+ (* i n) i) 1)
        (recur (inc i))))
    (loop [r (persistent! ident) base m p l]
      (if (zero? p)
        r
        (let [nr (if (odd? p) (mat-mul r base n) r)
              np (bit-shift-right p 1)
              nbase (if (pos? np) (mat-mul base base n) base)]
          (recur nr nbase np))))))

(defn- count-strings [regex l]
  (let [ast (parse-regex regex)
        adj (transient [])
        [start-nfa end-nfa] (build-nfa ast adj)
        p-adj (persistent! adj)
        num-states (count p-adj)
        start-set (eclosure-set p-adj num-states [start-nfa])
        states (transient [start-set])
        id-map (transient {start-set 0})
        dfa-adj (transient [])]
    (loop [head 0]
      (when (< head (count states))
        (let [cur-set (nth states head)
              to1 (let [m (move-set p-adj cur-set 1)]
                    (if (seq m) (eclosure-set p-adj num-states m) []))
              to2 (let [m (move-set p-adj cur-set 2)]
                    (if (seq m) (eclosure-set p-adj num-states m) []))
              nxt1 (if-let [id (get id-map to1)]
                     id
                     (let [id (count states)]
                       (conj! states to1)
                       (assoc! id-map to1 id)
                       id))
              nxt2 (if-let [id (get id-map to2)]
                     id
                     (let [id (count states)]
                       (conj! states to2)
                       (assoc! id-map to2 id)
                       id))]
          (conj! dfa-adj [nxt1 nxt2])
          (recur (inc head)))))
    (let [sz (count states)
          p-dfa (persistent! dfa-adj)
          mat (transient (vec (repeat (* sz sz) 0)))]
      (loop [i 0]
        (when (< i sz)
          (let [[n1 n2] (nth p-dfa i)
                i-off (* i sz)
                idx1 (+ i-off n1)
                idx2 (+ i-off n2)]
            (assoc! mat idx1 (rem (inc (nth mat idx1)) MOD))
            (assoc! mat idx2 (rem (inc (nth mat idx2)) MOD))
            (recur (inc i)))))
      (let [mp (mat-pow (persistent! mat) l sz)
            p-states (persistent! states)]
        (loop [i 0 ans 0]
          (if (= i sz)
            ans
            (let [st (nth p-states i)]
              (if (some #(= % end-nfa) st)
                (recur (inc i) (rem (+ ans (nth mp i)) MOD))
                (recur (inc i) ans)))))))))

(defn -main []
  (try
    (let [content (slurp *in*)]
      (when-not (str/blank? content)
        (let [toks (tokenize content)
              nt (count toks)]
          (when (pos? nt)
            (let [t (parse-long (nth toks 0))]
              (loop [i 0 idx 1]
                (when (< i t)
                  (let [r (nth toks idx)
                        l (parse-long (nth toks (inc idx)))]
                    (println (count-strings r l))
                    (recur (inc i) (+ idx 2))))))))))
    (catch Exception _
      (binding [*out* *err*] (println "error: invalid input")))))

(-main)
