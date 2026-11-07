;; String function calculation: max over all distinct substrings of
;; length times occurrences.
;; Input: a single lowercase string token. Output: the maximum.
;; Linear expected time and space with a suffix automaton stored in
;; flat transient int vectors plus a linear counting sort of states
;; by length for occurrence flow.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (if (nil? s)
    []
    (let [v (clojure.string/split s #"[ \t\n\r\f]+")
          n (count v)
          start (if (and (pos? n) (str/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (str/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- lower-string?
  "True when every char of s is a lowercase letter."
  [s]
  (let [n (count s)]
    (and (pos? n)
         (loop [i 0]
           (if (= i n)
             true
             (let [c (int (nth s i))]
               (if (and (>= c 97) (<= c 122))
                 (recur (inc i))
                 false)))))))

(defn- find-next
  "Target state from u on char code c, or -1."
  [head-v edge-ch edge-to edge-next u c]
  (loop [e (nth head-v u)]
    (cond
      (= e -1) -1
      (= (nth edge-ch e) c) (nth edge-to e)
      :else (recur (nth edge-next e)))))

(defn- add-edge!
  "Prepends transition (u, c) -> target."
  [head-v edge-ch edge-to edge-next u c target]
  (let [e (count edge-ch)]
    (conj! edge-ch c)
    (conj! edge-to target)
    (conj! edge-next (nth head-v u))
    (assoc! head-v u e)))

(defn- set-edge!
  "Updates existing transition (u, c) to target."
  [head-v edge-ch edge-to edge-next u c target]
  (loop [e (nth head-v u)]
    (when (not= e -1)
      (if (= (nth edge-ch e) c)
        (assoc! edge-to e target)
        (recur (nth edge-next e))))))

(defn- string-value
  "Max over distinct substrings of length times occurrences."
  [s]
  (let [n (count s)]
    (if (zero? n)
      0
      (let [len-v (transient [0])
            link-v (transient [-1])
            occ-v (transient [0])
            head-v (transient [-1])
            edge-ch (transient [])
            edge-to (transient [])
            edge-next (transient [])]
        (loop [i 0 last-idx 0]
          (when (< i n)
            (let [c (- (int (nth s i)) 97)
                  cur (count len-v)
                  _ (do (conj! len-v (inc (nth len-v last-idx)))
                        (conj! link-v 0)
                        (conj! occ-v 1)
                        (conj! head-v -1))
                  p (loop [p last-idx]
                      (if (or (= p -1)
                              (not= (find-next head-v edge-ch edge-to edge-next p c) -1))
                        p
                        (do
                          (add-edge! head-v edge-ch edge-to edge-next p c cur)
                          (recur (nth link-v p)))))]
              (if (= p -1)
                (assoc! link-v cur 0)
                (let [q (find-next head-v edge-ch edge-to edge-next p c)
                      lp (nth len-v p)
                      lq (nth len-v q)]
                  (if (= (inc lp) lq)
                    (assoc! link-v cur q)
                    (let [clone (count len-v)]
                      (conj! len-v (inc lp))
                      (conj! link-v (nth link-v q))
                      (conj! occ-v 0)
                      (conj! head-v -1)
                      (loop [e (nth head-v q)]
                        (when (not= e -1)
                          (add-edge! head-v edge-ch edge-to edge-next
                                     clone (nth edge-ch e) (nth edge-to e))
                          (recur (nth edge-next e))))
                      (loop [p2 p]
                        (when (and (not= p2 -1)
                                   (= (find-next head-v edge-ch edge-to edge-next p2 c) q))
                          (set-edge! head-v edge-ch edge-to edge-next p2 c clone)
                          (recur (nth link-v p2))))
                      (assoc! link-v q clone)
                      (assoc! link-v cur clone)))))
              (recur (inc i) cur))))
        (let [nst (count len-v)
              len-head (transient (vec (repeat (inc n) -1)))
              len-next (transient (vec (repeat nst -1)))]
          (loop [v 0]
            (when (< v nst)
              (let [l (nth len-v v)]
                (assoc! len-next v (nth len-head l))
                (assoc! len-head l v)
                (recur (inc v)))))
          (loop [l n ans n]
            (if (zero? l)
              ans
              (let [ans2 (loop [v (nth len-head l) best ans]
                           (if (= v -1)
                             best
                             (let [oc (nth occ-v v)
                                   lk (nth link-v v)
                                   prod (*' l oc)]
                               (when (pos? lk)
                                 (assoc! occ-v lk (+' (nth occ-v lk) oc)))
                               (recur (nth len-next v) (if (> prod best) prod best)))))]
                (recur (dec l) ans2)))))))))

(defn -main
  "Parses data and prints the string function value.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)
            s (first toks)]
        (if (or (nil? s) (not (lower-string? s)))
          (report "error: expected a single lowercase string token")
          (println (string-value s))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
