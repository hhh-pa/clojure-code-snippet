(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n-nodes (Integer/parseInt (nth toks 0))
          n-events (Integer/parseInt (nth toks 1))
          roles (make-array String n-nodes)
          _ (loop [i 0] (when (< i n-nodes) (aset roles i "FOLLOWER") (recur (inc i))))
          ^ints current-term (int-array n-nodes)
          ^ints voted-for (int-array n-nodes -1)
          ^ints commit-index (int-array n-nodes -1)
          leader-log-sz (int-array 1)
          majority (inc (quot n-nodes 2))
          sb (StringBuilder.)]

      (loop [e 0, ti 2]
        (when (and (< e n-events) (< ti nt))
          (let [event-type (nth toks ti)]
            (cond
              (= event-type "START_ELECTION")
              (let [cand-id (Integer/parseInt (nth toks (+ ti 1)))]
                (aset roles cand-id "CANDIDATE")
                (let [cand-term (inc (aget current-term cand-id))]
                  (aset current-term cand-id cand-term)
                  (aset voted-for cand-id cand-id)
                  (let [votes (loop [i 0, cnt 1]
                                (if (< i n-nodes)
                                  (if (== i cand-id)
                                    (recur (inc i) cnt)
                                    (if (< (aget current-term i) cand-term)
                                      (do
                                        (aset current-term i cand-term)
                                        (aset roles i "FOLLOWER")
                                        (aset voted-for i cand-id)
                                        (recur (inc i) (inc cnt)))
                                      (recur (inc i) cnt)))
                                  cnt))]
                    (if (>= votes majority)
                      (do
                        (aset roles cand-id "LEADER")
                        (.append sb (str "LEADER ELECTED: " cand-id " TERM: " cand-term "\n")))
                      (do
                        (aset roles cand-id "FOLLOWER")
                        (.append sb (str "ELECTION FAILED: " cand-id "\n"))))))
                (recur (inc e) (+ ti 2)))

              (= event-type "APPEND_ENTRY")
              (let [leader-id (Integer/parseInt (nth toks (+ ti 1)))
                    cmd (nth toks (+ ti 2))]
                (if (not= (aget roles leader-id) "LEADER")
                  (.append sb "REJECT NOT LEADER\n")
                  (let [l-term (aget current-term leader-id)
                        new-sz (inc (aget leader-log-sz 0))]
                    (aset leader-log-sz 0 new-sz)
                    (let [replicated (loop [i 0, cnt 1]
                                       (if (< i n-nodes)
                                         (if (== i leader-id)
                                           (recur (inc i) cnt)
                                           (do
                                             (aset current-term i l-term)
                                             (aset roles i "FOLLOWER")
                                             (recur (inc i) (inc cnt))))
                                         cnt))]
                      (when (>= replicated majority)
                        (let [commit-idx (dec new-sz)]
                          (loop [i 0]
                            (when (< i n-nodes)
                              (aset commit-index i commit-idx)
                              (recur (inc i))))
                          (.append sb (str "COMMITTED " cmd " AT INDEX " commit-idx "\n")))))))
                (recur (inc e) (+ ti 3)))

              (= event-type "PRINT_STATE")
              (let [node-id (Integer/parseInt (nth toks (+ ti 1)))]
                (.append sb (str "NODE " node-id " ROLE " (aget roles node-id) " TERM " (aget current-term node-id) " COMMITTED " (aget commit-index node-id) "\n"))
                (recur (inc e) (+ ti 2)))

              :else
              (recur (inc e) (inc ti))))))
      (print (.toString sb)))))
