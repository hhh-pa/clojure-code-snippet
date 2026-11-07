;; String function calculation with a suffix tree: max over all
;; distinct substrings of length times occurrences.
;; Input: a single lowercase string token. Output: the maximum.
;; Linear time and space with the online tree method over the string
;; plus a terminal mark; the answer is the max over tree nodes of
;; string depth times subtree leaf count.
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

(defn- find-edge
  "Edge index from node u on char code c, or -1."
  [head-v edges u c]
  (loop [e (nth head-v u)]
    (if (= e -1)
      -1
      (let [ev (nth edges e)]
        (if (= (bit-shift-right ev 48) c)
          e
          (recur (dec (bit-and (bit-shift-right ev 24) 16777215))))))))

(defn- string-value
  "Max over distinct substrings of length times occurrences."
  [s]
  (let [n (count s)]
    (if (zero? n)
      0
      (let [t (str s "$")
            nt (inc n)
            link-v (transient [0])
            start-v (transient [0])
            end-v (transient [-2])
            head-v (transient [-1])
            edges (transient [])]
        (loop [pos 0 active 0 edge 0 alen 0 rem 1 last-new -1]
          (when (< pos nt)
            (if (zero? rem)
              (recur (inc pos) active edge alen 1 -1)
              (let [pos-ch (int (nth t pos))
                    edge (if (zero? alen) pos edge)
                    cur-char (int (nth t edge))
                    e (find-edge head-v edges active cur-char)]
                (if (= e -1)
                  (let [leaf (count link-v)
                        ne (count edges)
                        h0 (nth head-v active)]
                    (conj! link-v 0)
                    (conj! start-v pos)
                    (conj! end-v -1)
                    (conj! head-v -1)
                    (conj! edges (bit-or leaf
                                         (bit-shift-left (inc h0) 24)
                                         (bit-shift-left cur-char 48)))
                    (assoc! head-v active ne)
                    (when (not= last-new -1)
                      (assoc! link-v last-new active))
                    (let [rem-next (dec rem)]
                      (if (zero? active)
                        (if (pos? alen)
                          (recur pos 0 (inc (- pos rem-next)) (dec alen) rem-next -1)
                          (recur pos 0 edge alen rem-next -1))
                        (recur pos (nth link-v active) edge alen rem-next -1))))
                  (let [ev (nth edges e)
                        nxt (bit-and ev 16777215)
                        en (nth end-v nxt)
                        nxt-start (nth start-v nxt)
                        elen (inc (- (if (= en -1) pos en) nxt-start))]
                    (if (>= alen elen)
                      (recur pos nxt (+ edge elen) (- alen elen) rem last-new)
                      (let [epos (+ nxt-start alen)
                            epos-ch (int (nth t epos))]
                        (if (= epos-ch pos-ch)
                          (do
                            (when (and (not= last-new -1) (not (zero? active)))
                              (assoc! link-v last-new active))
                            (recur (inc pos) active edge (inc alen) (inc rem) -1))
                          (let [split-end (+ nxt-start alen -1)
                                split (count link-v)
                                leaf (inc split)
                                ne (count edges)]
                            (conj! link-v 0)
                            (conj! start-v nxt-start)
                            (conj! end-v split-end)
                            (conj! head-v (inc ne))
                            (conj! link-v 0)
                            (conj! start-v pos)
                            (conj! end-v -1)
                            (conj! head-v -1)
                            (assoc! edges e (bit-or (bit-and ev -16777216) split))
                            (conj! edges (bit-or leaf (bit-shift-left pos-ch 48)))
                            (conj! edges (bit-or nxt
                                                 (bit-shift-left (inc ne) 24)
                                                 (bit-shift-left epos-ch 48)))
                            (assoc! start-v nxt epos)
                            (when (not= last-new -1)
                              (assoc! link-v last-new split))
                            (let [rem-next (dec rem)]
                              (if (zero? active)
                                (if (pos? alen)
                                  (recur pos 0 (inc (- pos rem-next)) (dec alen) rem-next split)
                                  (recur pos 0 edge alen rem-next split))
                                (recur pos (nth link-v active) edge alen rem-next split)))))))))))))
        (let [nst (count link-v)
              order (transient [0])
              cnt (transient (vec (repeat nst 0)))]
          (assoc! link-v 0 0)
          (loop [qi 0]
            (when (< qi (count order))
              (let [v (nth order qi)
                    dv (nth link-v v)
                    leaves (loop [e (nth head-v v) lc 0]
                             (if (= e -1)
                               lc
                               (let [ev (nth edges e)
                                     w (bit-and ev 16777215)
                                     en (nth end-v w)
                                     ne (dec (bit-and (bit-shift-right ev 24) 16777215))]
                                 (if (= en -1)
                                   (recur ne (inc lc))
                                   (let [w-len (inc (- en (nth start-v w)))]
                                     (assoc! start-v w v)
                                     (assoc! link-v w (+ dv w-len))
                                     (conj! order w)
                                     (recur ne lc))))))]
                (assoc! cnt v leaves)
                (recur (inc qi)))))
          (loop [qi (dec (count order)) ans n]
            (if (<= qi 0)
              ans
              (let [v (nth order qi)
                    c (nth cnt v)
                    p (nth start-v v)
                    _ (assoc! cnt p (+ (nth cnt p) c))
                    prod (* (nth link-v v) c)]
                (recur (dec qi) (if (> prod ans) prod ans))))))))))

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
