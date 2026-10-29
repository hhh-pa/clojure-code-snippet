;; String function calculation: max over all distinct substrings of
;; length times occurrences.
;; Input: a single lowercase string token. Output: the maximum.
;; Linear expected time and space with a suffix automaton plus a
;; linear counting sort of states by length for occurrence flow.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def alpha-index
  "Lowercase chars mapped to their slot, used for validation."
  (zipmap "abcdefghijklmnopqrstuvwxyz" (range 26)))

(defn- blank-char?
  "True for the whitespace chars that split tokens."
  [c]
  (or (= c \space) (= c \tab) (= c \newline) (= c \return) (= c \formfeed)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [c (first cs)]
        (if (blank-char? c)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur c) toks))))))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- lower-string?
  "True when every char of s is a lowercase letter."
  [s]
  (and (seq s) (every? #(contains? alpha-index %) s)))

(defn- sam-extend
  "Extends automaton [states last-idx] with char c, cloning a state
  when the old transition skips lengths. Returns [states cur]."
  [[states last-idx] c]
  (let [cur (count states)
        states (conj states {:len (inc (:len (states last-idx)))
                             :link 0
                             :next {}
                             :occ 1})
        [states p] (loop [states states
                          p last-idx]
                     (if (or (= p -1) (contains? (:next (states p)) c))
                       [states p]
                       (recur (assoc-in states [p :next c] cur)
                              (:link (states p)))))]
    (if (= p -1)
      [(assoc-in states [cur :link] 0) cur]
      (let [q (get-in states [p :next c])]
        (if (= (inc (:len (states p))) (:len (states q)))
          [(assoc-in states [cur :link] q) cur]
          (let [clone (count states)
                states (conj states (assoc (states q)
                                           :len (inc (:len (states p)))
                                           :occ 0))
                states (loop [states states
                               p p]
                         (if (and (not (= p -1))
                                  (= (get-in states [p :next c]) q))
                           (recur (assoc-in states [p :next c] clone)
                                  (:link (states p)))
                           states))]
            [(-> states
                 (assoc-in [q :link] clone)
                 (assoc-in [cur :link] clone))
             cur]))))))

(defn- build-sam
  "Suffix automaton states for char seq s with end counts."
  [s]
  (first (reduce sam-extend [[{:len 0 :link -1 :next {} :occ 0}] 0] s)))

(defn- string-value
  "Max over distinct substrings of length times occurrences."
  [s]
  (let [n (count s)]
    (if (zero? n)
      0
      (let [states (build-sam s)
            nst (count states)
            buckets (reduce (fn [bk v]
                              (update bk (:len (nth states v)) conj v))
                            (vec (repeat (inc n) []))
                            (range nst))
            order (reverse (apply concat buckets))
            occ (persistent!
                 (reduce (fn [oc v]
                           (let [lk (:link (nth states v))]
                             (if (>= lk 0)
                               (assoc! oc lk (+' (nth oc lk) (nth oc v)))
                               oc)))
                         (transient (mapv :occ states))
                         order))]
        (reduce (fn [ans v]
                  (max ans (*' (:len (nth states v)) (nth occ v))))
                n
                (range nst))))))

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
          (println (string-value (vec s)))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
