(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n-procs (Integer/parseInt (nth toks 0))
          n-events (Integer/parseInt (nth toks 1))
          ^ints balance (int-array n-procs)
          _ (loop [i 0] (when (< i n-procs) (aset balance i (Integer/parseInt (nth toks (+ 2 i)))) (recur (inc i))))
          ^booleans recorded (boolean-array n-procs)
          ^ints rec-balance (int-array n-procs)
          channels (make-array Object n-procs n-procs)
          rec-channels (make-array Object n-procs n-procs)
          is-recording (make-array Boolean/TYPE n-procs n-procs)
          _ (loop [i 0]
              (when (< i n-procs)
                (loop [j 0]
                  (when (< j n-procs)
                    (aset channels i j (java.util.ArrayDeque.))
                    (aset rec-channels i j (java.util.ArrayList.))
                    (recur (inc j))))
                (recur (inc i))))
          sb (StringBuilder.)]

      (loop [e 0, ti (+ 2 n-procs)]
        (when (and (< e n-events) (< ti nt))
          (let [event-type (nth toks ti)]
            (cond
              (= event-type "TRANSFER")
              (let [u (Integer/parseInt (nth toks (+ ti 1)))
                    v (Integer/parseInt (nth toks (+ ti 2)))
                    amt (Integer/parseInt (nth toks (+ ti 3)))]
                (aset balance u (- (aget balance u) amt))
                (.add ^java.util.Queue (aget channels u v) (str amt))
                (recur (inc e) (+ ti 4)))

              (= event-type "RECEIVE")
              (let [u (Integer/parseInt (nth toks (+ ti 1)))
                    v (Integer/parseInt (nth toks (+ ti 2)))
                    ^java.util.Queue ch (aget channels u v)]
                (when-not (.isEmpty ch)
                  (let [msg (.poll ch)]
                    (if (= msg "MARKER")
                      (if-not (aget recorded v)
                        (do
                          (aset recorded v true)
                          (aset rec-balance v (aget balance v))
                          (loop [k 0]
                            (when (< k n-procs)
                              (when (and (not= k v) (not= k u))
                                (aset is-recording k v true))
                              (when (not= k v)
                                (.add ^java.util.Queue (aget channels v k) "MARKER"))
                              (recur (inc k)))))
                        (aset is-recording u v false))
                      (let [amt (Integer/parseInt msg)]
                        (aset balance v (+ (aget balance v) amt))
                        (when (aget is-recording u v)
                          (.add ^java.util.List (aget rec-channels u v) msg))))))
                (recur (inc e) (+ ti 3)))

              (= event-type "SNAPSHOT_INIT")
              (let [init-p (Integer/parseInt (nth toks (+ ti 1)))]
                (when-not (aget recorded init-p)
                  (aset recorded init-p true)
                  (aset rec-balance init-p (aget balance init-p))
                  (loop [k 0]
                    (when (< k n-procs)
                      (when (not= k init-p)
                        (aset is-recording k init-p true)
                        (.add ^java.util.Queue (aget channels init-p k) "MARKER"))
                      (recur (inc k)))))
                (recur (inc e) (+ ti 2)))

              :else
              (recur (inc e) (inc ti))))))

      (let [total-money (int-array 1)]
        (loop [i 0]
          (when (< i n-procs)
            (let [b (aget rec-balance i)]
              (.append sb (str "P" i ": " b "\n"))
              (aset total-money 0 (+ (aget total-money 0) b)))
            (recur (inc i))))

        (loop [u 0]
          (when (< u n-procs)
            (loop [v 0]
              (when (< v n-procs)
                (let [^java.util.List rch (aget rec-channels u v)
                      sz (.size rch)]
                  (loop [k 0]
                    (when (< k sz)
                      (let [msg (.get rch k)]
                        (.append sb (str "CHANNEL " u "->" v ": " msg "\n"))
                        (aset total-money 0 (+ (aget total-money 0) (Integer/parseInt msg))))
                      (recur (inc k)))))
                (recur (inc v))))
            (recur (inc u))))

        (.append sb (str "TOTAL: " (aget total-money 0) "\n"))
        (print (.toString sb))))))
