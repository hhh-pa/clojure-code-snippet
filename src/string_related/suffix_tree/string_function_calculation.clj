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

(defn- find-next
  "Child node of u on char code c, or -1."
  [head-v edge-ch edge-to edge-next u c]
  (loop [e (nth head-v u)]
    (cond
      (= e -1) -1
      (= (nth edge-ch e) c) (nth edge-to e)
      :else (recur (nth edge-next e)))))

(defn- set-next!
  "Sets transition (u, c) -> child, updating in place if present."
  [head-v edge-ch edge-to edge-next u c child]
  (loop [e (nth head-v u)]
    (if (= e -1)
      (let [ne (count edge-ch)]
        (conj! edge-ch c)
        (conj! edge-to child)
        (conj! edge-next (nth head-v u))
        (assoc! head-v u ne))
      (if (= (nth edge-ch e) c)
        (assoc! edge-to e child)
        (recur (nth edge-next e))))))

(defn- edge-len
  "Length of the edge into node u with leaves open to leaf-end."
  [start-v end-v leaf-v u leaf-end]
  (if (zero? u)
    0
    (let [en (if (= (nth leaf-v u) 1) leaf-end (nth end-v u))]
      (inc (- en (nth start-v u))))))

(defn- add-node!
  "Appends a node and returns its new index."
  [link-v start-v end-v leaf-v head-v start end leaf-flag]
  (let [id (count link-v)]
    (conj! link-v 0)
    (conj! start-v start)
    (conj! end-v end)
    (conj! leaf-v leaf-flag)
    (conj! head-v -1)
    id))

(defn- follow-link
  "Active point step after an extension, following suffix links."
  [link-v active edge alen pos rem]
  (cond
    (and (zero? active) (pos? alen)) [active (inc (- pos rem)) (dec alen)]
    (zero? active) [active edge alen]
    :else [(nth link-v active) edge alen]))

(defn- string-value
  "Max over distinct substrings of length times occurrences."
  [s]
  (let [n (count s)]
    (if (zero? n)
      0
      (let [t (str s "$")
            nt (count t)
            link-v (transient [0])
            start-v (transient [0])
            end-v (transient [-1])
            leaf-v (transient [0])
            head-v (transient [-1])
            edge-ch (transient [])
            edge-to (transient [])
            edge-next (transient [])]
        (loop [pos 0 active 0 edge 0 alen 0 rem 0]
          (when (< pos nt)
            (let [pos-ch (int (nth t pos))
                  [active2 edge2 alen2 rem2]
                  (loop [active active edge edge alen alen rem (inc rem) last-new -1]
                    (if (zero? rem)
                      [active edge alen rem]
                      (let [edge (if (zero? alen) pos edge)
                            cur-char (int (nth t edge))
                            nxt (find-next head-v edge-ch edge-to edge-next active cur-char)]
                        (if (= nxt -1)
                          (let [leaf (add-node! link-v start-v end-v leaf-v head-v pos -1 1)]
                            (set-next! head-v edge-ch edge-to edge-next active cur-char leaf)
                            (when (not= last-new -1)
                              (assoc! link-v last-new active))
                            (let [rem-next (dec rem)
                                  [na ne nal] (follow-link link-v active edge alen pos rem-next)]
                              (recur na ne nal rem-next -1)))
                          (let [elen (edge-len start-v end-v leaf-v nxt pos)]
                            (if (>= alen elen)
                              (recur nxt (+ edge elen) (- alen elen) rem last-new)
                              (let [nxt-start (nth start-v nxt)
                                    epos (+ nxt-start alen)
                                    epos-ch (int (nth t epos))]
                                (if (= epos-ch pos-ch)
                                  (do
                                    (when (and (not= last-new -1) (not (zero? active)))
                                      (assoc! link-v last-new active))
                                    [active edge (inc alen) rem])
                                  (let [split-end (+ nxt-start alen -1)
                                        split (add-node! link-v start-v end-v leaf-v head-v
                                                         nxt-start split-end 0)
                                        leaf (add-node! link-v start-v end-v leaf-v head-v
                                                        pos -1 1)]
                                    (set-next! head-v edge-ch edge-to edge-next active cur-char split)
                                    (set-next! head-v edge-ch edge-to edge-next split pos-ch leaf)
                                    (assoc! start-v nxt (+ nxt-start alen))
                                    (set-next! head-v edge-ch edge-to edge-next split epos-ch nxt)
                                    (when (not= last-new -1)
                                      (assoc! link-v last-new split))
                                    (let [rem-next (dec rem)
                                          [na ne nal] (follow-link link-v active edge alen pos rem-next)]
                                      (recur na ne nal rem-next split)))))))))))]
              (recur (inc pos) active2 edge2 alen2 rem2))))
        (let [nst (count link-v)
              order (transient [0])
              depth (transient (vec (repeat nst 0)))
              cnt (transient (vec (repeat nst 0)))]
          (loop [qi 0]
            (when (< qi nst)
              (let [v (nth order qi)
                    dv (nth depth v)]
                (loop [e (nth head-v v)]
                  (when (not= e -1)
                    (let [w (nth edge-to e)
                          w-len (if (= (nth leaf-v w) 1)
                                  (- n (nth start-v w))
                                  (inc (- (nth end-v w) (nth start-v w))))]
                      (conj! order w)
                      (assoc! depth w (+ dv w-len))
                      (recur (nth edge-next e)))))
                (recur (inc qi)))))
          (loop [qi (dec nst) ans n]
            (if (neg? qi)
              ans
              (let [v (nth order qi)
                    c (if (= (nth leaf-v v) 1)
                        1
                        (loop [e (nth head-v v) sum 0]
                          (if (= e -1)
                            sum
                            (recur (nth edge-next e)
                                   (+ sum (nth cnt (nth edge-to e)))))))
                    _ (assoc! cnt v c)
                    prod (* (nth depth v) c)]
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
