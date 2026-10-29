(ns compiler.gvn
  "Global value numbering over straight-line three-address code.

   Time: O(n) expected (persistent hash-map lookups/inserts per
   instruction; HEAD used Java HashMaps for the same bound). Space: O(n)
   value numbers and expression keys. Hash-consing is asymptotically
   optimal for local value numbering."
  (:refer-clojure :exclude [parse-long])
  (:require [clojure.string :as str]))

(def digit-values
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


(defn parse-long
  [s]
  (when (and (string? s) (seq s))
    (let [cs (seq s)
          [neg? rest-cs] (cond
                           (= (first cs) \-) [true (next cs)]
                           (= (first cs) \+) [false (next cs)]
                           :else [false cs])]
      (when (seq rest-cs)
        (loop [cs rest-cs
               acc 0]
          (if (nil? cs)
            (fit-long (if neg? (-' acc) acc))
            (let [d (get digit-values (first cs))]
              (when-not (nil? d)
                (recur (next cs) (+' (*' acc 10) d))))))))))

(defn error
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn get-var-vn
  "Look up variable `v`, allocating a fresh VN on first use. Returns
   [state vn]."
  [state v]
  (if-let [vn (get-in state [:var->vn v])]
    [state vn]
    (let [vn (:next state)]
      [(-> state
           (assoc-in [:var->vn v] vn)
           (assoc-in [:vn->rep vn] v)
           (update :next inc))
       vn])))

(defn process-inst
  "Process one instruction, printing its numbered form. Returns new state."
  [state {:keys [dest opcode arg1 arg2]}]
  (cond
    (= opcode "CONST")
    (let [val (parse-long arg1)]
      (if (nil? val)
        state
        (if-let [vn (get-in state [:const->vn val])]
          (do
            (println (str dest " = " (get-in state [:vn->rep vn]) " (VN" vn " REDUNDANT)"))
            (-> state
                (assoc-in [:var->vn dest] vn)
                (update :redundant inc)))
          (let [vn (:next state)]
            (println (str dest " = CONST " val " (VN" vn ")"))
            (-> state
                (assoc-in [:const->vn val] vn)
                (assoc-in [:var->vn dest] vn)
                (assoc-in [:vn->rep vn] dest)
                (update :next inc))))))

    (= opcode "COPY")
    (let [[state src-vn] (get-var-vn state arg1)]
      (println (str dest " = " (get-in state [:vn->rep src-vn])
                    " (VN" src-vn " REDUNDANT)"))
      (-> state
          (assoc-in [:var->vn dest] src-vn)
          (update :redundant inc)))

    (= opcode "PHI")
    (let [[state vn1] (get-var-vn state arg1)
          [state vn2] (get-var-vn state arg2)]
      (if (= vn1 vn2)
        (do
          (println (str dest " = " (get-in state [:vn->rep vn1])
                        " (VN" vn1 " REDUNDANT PHI)"))
          (-> state
              (assoc-in [:var->vn dest] vn1)
              (update :redundant inc)))
        (let [min-vn (min vn1 vn2)
              max-vn (max vn1 vn2)
              expr-key ["PHI" min-vn max-vn]]
          (if-let [vn (get-in state [:expr->vn expr-key])]
            (do
              (println (str dest " = " (get-in state [:vn->rep vn])
                            " (VN" vn " REDUNDANT)"))
              (-> state
                  (assoc-in [:var->vn dest] vn)
                  (update :redundant inc)))
            (let [vn (:next state)]
              (println (str dest " = PHI VN" min-vn " VN" max-vn " (VN" vn ")"))
              (-> state
                  (assoc-in [:expr->vn expr-key] vn)
                  (assoc-in [:var->vn dest] vn)
                  (assoc-in [:vn->rep vn] dest)
                  (update :next inc)))))))

    :else
    (let [[state vn1] (get-var-vn state arg1)
          [state vn2] (get-var-vn state arg2)
          [vn1 vn2] (if (and (or (= opcode "ADD") (= opcode "MUL")) (> vn1 vn2))
                      [vn2 vn1]
                      [vn1 vn2])
          expr-key [opcode vn1 vn2]]
      (if-let [vn (get-in state [:expr->vn expr-key])]
        (do
          (println (str dest " = " (get-in state [:vn->rep vn])
                        " (VN" vn " REDUNDANT)"))
          (-> state
              (assoc-in [:var->vn dest] vn)
              (update :redundant inc)))
        (let [vn (:next state)]
          (println (str dest " = " opcode " VN" vn1 " VN" vn2 " (VN" vn ")"))
          (-> state
              (assoc-in [:expr->vn expr-key] vn)
              (assoc-in [:var->vn dest] vn)
              (assoc-in [:vn->rep vn] dest)
              (update :next inc)))))))

(defn parse-insts
  "Parse `n` instructions from token seq. Returns [insts ok?]."
  [toks n]
  (loop [rem toks
         acc []
         i 0]
    (if (>= i n)
      [acc true]
      (if (< (count rem) 3)
        [acc false]
        (let [dest (first rem)
              opcode (nth rem 2)]
          (if (or (= opcode "CONST") (= opcode "COPY"))
            (if (< (count rem) 4)
              [acc false]
              (recur (drop 4 rem)
                     (conj acc {:dest dest :opcode opcode
                                :arg1 (nth rem 3) :arg2 ""})
                     (inc i)))
            (if (< (count rem) 5)
              [acc false]
              (recur (drop 5 rem)
                     (conj acc {:dest dest :opcode opcode
                                :arg1 (nth rem 3) :arg2 (nth rem 4)})
                     (inc i)))))))))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            n-inst (parse-long (get toks 0))]
        (cond
          (nil? n-inst)
          (error "invalid input: expected instruction count")

          (neg? n-inst)
          (error "invalid input: negative instruction count")

          :else
          (let [[insts ok?] (parse-insts (vec (rest toks)) n-inst)]
            (if-not ok?
              (error "invalid input: truncated instruction list")
              (if (some #(and (= (:opcode %) "CONST")
                              (nil? (parse-long (:arg1 %))))
                        insts)
                (error "invalid input: bad CONST literal")
                (let [init {:var->vn {} :vn->rep {} :expr->vn {}
                            :const->vn {} :next 1 :redundant 0}
                      final (reduce process-inst init insts)]
                  (println (str "Total unique VNs: " (dec (:next final))))
                  (println (str "Redundant instructions eliminated: "
                                (:redundant final))))))))))))

(-main)
