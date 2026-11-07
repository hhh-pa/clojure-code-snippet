;; Shortest reach in an unweighted graph with edge weight six.
;; A queue based breadth first search from the start node sets each distance
;; once, which takes O(V+E) time and space for V nodes and E links held in
;; flat transient integer vectors.
(require '[clojure.string :as str])

(def digit-values
  "Digit chars mapped to their numeric values for the pure number reader."
  (zipmap "0123456789" (range 10)))

(def max-long 9223372036854775807)
(def min-long -9223372036854775808)

(defn fit-long [n]
  (when (<= min-long n max-long) n))

(defn tokenize
  "Split string s on blanks into a vector of token strings in linear time."
  [s]
  (if (nil? s)
    []
    (let [v (str/split s #"[ \t\n\r]+")
          n (count v)
          start (if (and (pos? n) (str/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (str/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn parse-int-token
  "Read one signed integer token into a long, else nil for a bad token."
  [tok]
  (when (and (string? tok) (pos? (count tok)))
    (parse-long tok)))

(defn parse-all-ints
  "Read every token of s as a signed integer, else nil for a bad token."
  [s]
  (let [toks (tokenize s)
        nt (count toks)]
    (loop [i 0 out (transient [])]
      (if (= i nt)
        (persistent! out)
        (let [v (parse-int-token (nth toks i))]
          (if (nil? v)
            nil
            (recur (inc i) (conj! out v))))))))

(defn fail
  "Report a bad input shape on stderr and return nil with exit code zero."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn bfs-line
  "Builds adjacency lists in flat transient int vectors, runs BFS from start,
   and returns the space-separated distance string."
  [n all edge-base m start]
  (let [head (transient (vec (repeat n -1)))
        to (transient [])
        nxt (transient [])]
    (loop [i 0]
      (when (< i m)
        (let [idx (+ edge-base (* i 2))
              u (dec (nth all idx))
              v (dec (nth all (inc idx)))
              e1 (* i 2)
              e2 (inc e1)]
          (conj! to v)
          (conj! nxt (nth head u))
          (assoc! head u e1)
          (conj! to u)
          (conj! nxt (nth head v))
          (assoc! head v e2)
          (recur (inc i)))))
    (let [q (transient [start])
          dist (assoc! (transient (vec (repeat n -1))) start 0)]
      (loop [qi 0]
        (when (< qi (count q))
          (let [cur (nth q qi)
                nd (+ (nth dist cur) 6)]
            (loop [e (nth head cur)]
              (when (not= e -1)
                (let [nb (nth to e)]
                  (when (= (nth dist nb) -1)
                    (assoc! dist nb nd)
                    (conj! q nb))
                  (recur (nth nxt e)))))
            (recur (inc qi)))))
      (loop [i 0 out (transient [])]
        (if (= i n)
          (str/join " " (persistent! out))
          (if (= i start)
            (recur (inc i) out)
            (recur (inc i) (conj! out (str (nth dist i))))))))))

(defn -main
  "Entry point reading the whole stdin string, silent on blank input."
  [input]
  (when-not (str/blank? input)
    (let [all (parse-all-ints input)]
      (if (nil? all)
        (fail "bad input: tokens must all be integers")
        (let [len (count all)]
          (when (pos? len)
            (loop [p 1 left (nth all 0)]
              (when (and (pos? left) (< (inc p) len))
                (let [n (nth all p)
                      m (nth all (inc p))]
                  (if (or (<= n 0) (< m 0) (> (+' p 2 (*' m 2)) (dec len)))
                    (fail "bad input: query header or edges out of range")
                    (let [edge-base (+ p 2)
                          edge-end (+ edge-base (* m 2))
                          start-tok (nth all edge-end)
                          edges-ok? (loop [idx edge-base]
                                      (if (= idx edge-end)
                                        true
                                        (let [v (nth all idx)]
                                          (if (and (<= 1 v) (<= v n))
                                            (recur (inc idx))
                                            false))))]
                      (if (or (< start-tok 1) (> start-tok n) (not edges-ok?))
                        (fail "bad input: node ids out of range")
                        (do
                          (println (bfs-line n all edge-base m (dec start-tok)))
                          (recur (inc edge-end) (dec left)))))))))))))))

(-main (slurp *in*))
