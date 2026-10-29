(ns compiler.gvn
  (:require [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [n-inst (Long/parseLong (first tokens))
            tokens (rest tokens)

            insts
            (loop [rem tokens
                   acc []
                   i 0]
              (if (or (>= i n-inst) (empty? rem))
                acc
                (let [dest (first rem)
                      _eq (second rem)
                      opcode (nth rem 2)]
                  (if (or (= opcode "CONST") (= opcode "COPY"))
                    (let [arg (nth rem 3)]
                      (recur (drop 4 rem) (conj acc {:dest dest :opcode opcode :arg1 arg :arg2 ""}) (inc i)))
                    (let [a1 (nth rem 3)
                          a2 (nth rem 4)]
                      (recur (drop 5 rem) (conj acc {:dest dest :opcode opcode :arg1 a1 :arg2 a2}) (inc i)))))))]

        (let [var-to-vn (java.util.HashMap.)
              vn-to-rep (java.util.HashMap.)
              expr-to-vn (java.util.HashMap.)
              const-to-vn (java.util.HashMap.)
              next-vn (int-array [1])
              redundant-count (int-array [0])

              get-var-vn (fn [v]
                           (if (.containsKey var-to-vn v)
                             (.get var-to-vn v)
                             (let [vn (aget next-vn 0)]
                               (aset next-vn 0 (inc vn))
                               (.put var-to-vn v vn)
                               (.put vn-to-rep vn v)
                               vn)))]

          (doseq [{:keys [dest opcode arg1 arg2]} insts]
            (cond
              (= opcode "CONST")
              (let [val (Long/parseLong arg1)]
                (if (.containsKey const-to-vn val)
                  (let [vn (.get const-to-vn val)]
                    (.put var-to-vn dest vn)
                    (aset redundant-count 0 (inc (aget redundant-count 0)))
                    (println (str dest " = " (.get vn-to-rep vn) " (VN" vn " REDUNDANT)")))
                  (let [vn (aget next-vn 0)]
                    (aset next-vn 0 (inc vn))
                    (.put const-to-vn val vn)
                    (.put var-to-vn dest vn)
                    (.put vn-to-rep vn dest)
                    (println (str dest " = CONST " val " (VN" vn ")")))))

              (= opcode "COPY")
              (let [src-vn (get-var-vn arg1)]
                (.put var-to-vn dest src-vn)
                (aset redundant-count 0 (inc (aget redundant-count 0)))
                (println (str dest " = " (.get vn-to-rep src-vn) " (VN" src-vn " REDUNDANT)")))

              (= opcode "PHI")
              (let [vn1 (get-var-vn arg1)
                    vn2 (get-var-vn arg2)]
                (if (= vn1 vn2)
                  (do
                    (.put var-to-vn dest vn1)
                    (aset redundant-count 0 (inc (aget redundant-count 0)))
                    (println (str dest " = " (.get vn-to-rep vn1) " (VN" vn1 " REDUNDANT PHI)")))
                  (let [min-vn (min vn1 vn2)
                        max-vn (max vn1 vn2)
                        expr-key ["PHI" min-vn max-vn]]
                    (if (.containsKey expr-to-vn expr-key)
                      (let [vn (.get expr-to-vn expr-key)]
                        (.put var-to-vn dest vn)
                        (aset redundant-count 0 (inc (aget redundant-count 0)))
                        (println (str dest " = " (.get vn-to-rep vn) " (VN" vn " REDUNDANT)")))
                      (let [vn (aget next-vn 0)]
                        (aset next-vn 0 (inc vn))
                        (.put expr-to-vn expr-key vn)
                        (.put var-to-vn dest vn)
                        (.put vn-to-rep vn dest)
                        (println (str dest " = PHI VN" min-vn " VN" max-vn " (VN" vn ")")))))))

              :else
              (let [vn1 (get-var-vn arg1)
                    vn2 (get-var-vn arg2)
                    [vn1 vn2] (if (and (or (= opcode "ADD") (= opcode "MUL")) (> vn1 vn2))
                                [vn2 vn1]
                                [vn1 vn2])
                    expr-key [opcode vn1 vn2]]
                (if (.containsKey expr-to-vn expr-key)
                  (let [vn (.get expr-to-vn expr-key)]
                    (.put var-to-vn dest vn)
                    (aset redundant-count 0 (inc (aget redundant-count 0)))
                    (println (str dest " = " (.get vn-to-rep vn) " (VN" vn " REDUNDANT)")))
                  (let [vn (aget next-vn 0)]
                    (aset next-vn 0 (inc vn))
                    (.put expr-to-vn expr-key vn)
                    (.put var-to-vn dest vn)
                    (.put vn-to-rep vn dest)
                    (println (str dest " = " opcode " VN" vn1 " VN" vn2 " (VN" vn ")")))))))

          (println (str "Total unique VNs: " (dec (aget next-vn 0))))
          (println (str "Redundant instructions eliminated: " (aget redundant-count 0))))))))

(-main)
