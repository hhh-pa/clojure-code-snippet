;; Shortest reach in an unweighted graph with edge weight six.
;; A queue based breadth first search from the start node sets each distance
;; once, which takes O(V+E) time and space for V nodes and E links held in
;; adjacency plus queue plus distance vectors. Lost nodes keep minus one.
;; Bad input shapes print a note on stderr and exit zero, since a nonzero
;; exit needs host calls, which this file avoids on purpose.
(require '[clojure.string :as str])

(def digit-values
  "Digit chars mapped to their numeric values for the pure number reader."
  (zipmap "0123456789" (range 10)))

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn fit-long
  "Returns n when it fits in a signed 64-bit long, else nil."
  [n]
  (when (<= min-long n max-long) n))


(defn whitespace?
  "True for the four blank chars treated as token separators."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

(defn tokenize
  "Split string s on blanks into a vector of token strings in linear time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (whitespace? ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-int-token
  "Read one signed integer token into a long, else nil for a bad token."
  [tok]
  (let [cs (seq tok)]
    (when (seq cs)
      (let [f (first cs)
            neg? (= f \-)
            ds (if (or neg? (= f \+)) (next cs) cs)]
        (when (seq ds)
          (loop [ds ds acc 0]
            (if (nil? ds)
              (fit-long (if neg? (-' acc) acc))
              (let [ch (first ds)]
                (if (contains? digit-values ch)
                  (recur (next ds) (+' (*' acc 10) (get digit-values ch)))
                  nil)))))))))

(defn parse-all-ints
  "Read every token of s as a signed integer, else nil for a bad token."
  [s]
  (let [toks (tokenize s)]
    (loop [ts (seq toks) out []]
      (if (nil? ts)
        out
        (let [v (parse-int-token (first ts))]
          (if (nil? v)
            nil
            (recur (next ts) (conj out v))))))))

(defn fail
  "Report a bad input shape on stderr and return nil with exit code zero."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn bfs-dist
  "Breadth first distances from start over adj lists, edge weight six."
  [adj start]
  (let [n (count adj)]
    (loop [q (vector start) head 0 dist (assoc (vec (repeat n -1)) start 0)]
      (if (>= head (count q))
        dist
        (let [cur (nth q head)
              dc (nth dist cur)
              [q dist]
              (loop [nbs (nth adj cur) q q dist dist]
                (if (empty? nbs)
                  [q dist]
                  (let [nxt (first nbs)
                        nd (+ dc 6)]
                    (if (or (= (nth dist nxt) -1) (< nd (nth dist nxt)))
                      (recur (rest nbs) (conj q nxt) (assoc dist nxt nd))
                      (recur (rest nbs) q dist)))))]
          (recur q (inc head) dist))))))

(defn build-adj
  "Adjacency lists for n nodes from flat edge pair vector edges."
  [n edges m]
  (loop [i 0 adj (vec (repeat n []))]
    (if (>= i m)
      adj
      (let [u (dec (nth edges (* i 2)))
            v (dec (nth edges (inc (* i 2))))]
        (recur (inc i)
               (-> adj
                   (assoc u (conj (nth adj u) v))
                   (assoc v (conj (nth adj v) u))))))))

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
                    (let [edges (subvec all (+ p 2) (+ p 2 (* m 2)))
                          start-tok (nth all (+ p 2 (* m 2)))]
                      (if (or (< start-tok 1) (> start-tok n)
                              (not (every? (fn [v] (and (<= 1 v) (<= v n)))
                                           edges)))
                        (fail "bad input: node ids out of range")
                        (let [dist (bfs-dist (build-adj n edges m)
                                             (dec start-tok))
                              start (dec start-tok)
                              line (str/join " " (map dist (remove (fn [i]
                                                                     (= i start))
                                                                   (range n))))]
                          (println line)
                          (recur (+ p 3 (* m 2)) (dec left)))))))))))))))

;; Minimal top level driver.
(-main (slurp *in*))
