;; Shell job control (CS:APP ch 8 + Shell Lab).
(require '[clojure.string :as str])

(defn- job-fg
  [jobs]
  (some (fn [[jid j]] (when (= (:state j) "FG") jid)) jobs))

(defn- ev-run
  [jobs nxt pid cmd bg]
  (if (and (zero? bg) (some? (job-fg jobs)))
    [jobs nxt ["REJECTED"]]
    [(assoc jobs nxt {:pid pid :state (if (zero? bg) "FG" "BG") :cmd cmd})
     (inc nxt) [(str "JOB " nxt " " pid)]]))

(defn- ev-sig
  [jobs op]
  (let [f (job-fg jobs)]
    (if (nil? f)
      [jobs ["NOJOB"]]
      (if (= op "INT")
        [(dissoc jobs f) [(str "TERM " f)]]
        [(assoc-in jobs [f :state] "ST") [(str "STOP " f)]]))))

(defn- ev-fg
  [jobs jid]
  (cond
    (not (contains? jobs jid)) [jobs ["NOJOB"]]
    (let [f (job-fg jobs)] (and (some? f) (not= f jid))) [jobs ["REJECTED"]]
    :else [(assoc-in jobs [jid :state] "FG") [(str "FG " jid)]]))

(defn- ev-bg
  [jobs jid]
  (cond
    (not (contains? jobs jid)) [jobs ["NOJOB"]]
    (= (:state (get jobs jid)) "FG") [jobs ["REJECTED"]]
    :else [(assoc-in jobs [jid :state] "BG") [(str "BG " jid)]]))

(defn- ev-kill
  [jobs jid]
  (if (contains? jobs jid)
    [(dissoc jobs jid) [(str "TERM " jid)]]
    [jobs ["NOJOB"]]))

(defn- ev-exit
  [jobs pid]
  (let [hit (some (fn [[jid j]] (when (= (:pid j) pid) jid)) jobs)]
    (if (nil? hit)
      [jobs ["NOJOB"]]
      [(dissoc jobs hit) [(str "DONE " hit)]])))

(defn- ev-jobs
  [jobs]
  (if (empty? jobs)
    ["NOJOBS"]
    (mapv (fn [[jid j]] (str jid " " (:pid j) " " (:state j) " " (:cmd j))) jobs)))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          e (Long/parseLong (next-tok))]
      (loop [i 0 jobs (sorted-map) nxt 1 out []]
        (if (>= i e)
          (print (str/join "\n" out))
          (let [op (next-tok)]
            (cond
              (= op "RUN") (let [pid (Long/parseLong (next-tok))
                                 cmd (next-tok)
                                 bg (Long/parseLong (next-tok))
                                 [nj nn lines] (ev-run jobs nxt pid cmd bg)]
                             (recur (inc i) nj nn (into out lines)))
              (= op "INT") (let [[nj lines] (ev-sig jobs op)]
                             (recur (inc i) nj nxt (into out lines)))
              (= op "TSTP") (let [[nj lines] (ev-sig jobs op)]
                              (recur (inc i) nj nxt (into out lines)))
              (= op "FG") (let [[nj lines] (ev-fg jobs (Long/parseLong (next-tok)))]
                            (recur (inc i) nj nxt (into out lines)))
              (= op "BG") (let [[nj lines] (ev-bg jobs (Long/parseLong (next-tok)))]
                            (recur (inc i) nj nxt (into out lines)))
              (= op "KILL") (let [[nj lines] (ev-kill jobs (Long/parseLong (next-tok)))]
                              (recur (inc i) nj nxt (into out lines)))
              (= op "EXIT") (let [[nj lines] (ev-exit jobs (Long/parseLong (next-tok)))]
                              (recur (inc i) nj nxt (into out lines)))
              :else (recur (inc i) jobs nxt (into out (ev-jobs jobs))))))))))
