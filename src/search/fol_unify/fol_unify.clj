
;; fol_unify: first-order unification with occurs check. Lowercase-led
;; symbols are variables; uppercase/digit-led are constants, or function
;; symbols when followed by '('. Prints the MGU sorted by variable.
(ns fol-unify
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn parse-term [s p]
  (let [q (loop [i p]
            (if (and (< i (count s)) (Character/isLetterOrDigit (.charAt s i)))
              (recur (inc i)) i))
        sym (subs s p q)]
    (if (and (< q (count s)) (= (.charAt s q) \( ))
      (loop [i (inc q) args []]
        (let [[t j] (parse-term s i)
              args2 (conj args t)]
          (if (= (.charAt s j) \,)
            (recur (inc j) args2)
            [{:sym sym :args args2 :isvar false} (inc j)])))
      [{:sym sym :args [] :isvar (Character/isLowerCase (.charAt sym 0))} q])))

(defn show* [t]
  (if (empty? (:args t))
    (:sym t)
    (str (:sym t) "(" (str/join "," (map show* (:args t))) ")")))

(declare subst*)

(defn subst* [t th]
  (if (:isvar t)
    (if-let [v (get @th (:sym t))]
      (let [r (subst* v th)]
        (swap! th assoc (:sym t) r)
        r)
      t)
    (assoc t :args (mapv #(subst* % th) (:args t)))))

(defn occurs* [v t th]
  (let [s (subst* t th)]
    (if (:isvar s)
      (= (:sym s) v)
      (boolean (some #(occurs* v % th) (:args s))))))

(defn unify* [a b th]
  (let [sa (subst* a th) sb (subst* b th)]
    (cond (and (:isvar sa) (:isvar sb) (= (:sym sa) (:sym sb))) true
          (:isvar sa) (if (occurs* (:sym sa) sb th)
                        false
                        (do (swap! th assoc (:sym sa) sb) true))
          (:isvar sb) (if (occurs* (:sym sb) sa th)
                        false
                        (do (swap! th assoc (:sym sb) sa) true))
          (or (not= (:sym sa) (:sym sb)) (not= (count (:args sa)) (count (:args sb)))) false
          :else (loop [as (:args sa) bs (:args sb)]
                  (if (empty? as)
                    true
                    (if (unify* (first as) (first bs) th)
                      (recur (rest as) (rest bs))
                      false))))))

(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (loop [pos 1 tcases (parse-token (first toks)) sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[t1 _] (parse-term (nth toks pos) 0)
              [t2 _] (parse-term (nth toks (inc pos)) 0)
              th (atom {})
              ok (unify* t1 t2 th)]
          (if (not ok)
            (.append sb "UNIFY FAIL\n")
            (let [ks (sort (keys @th))]
              (.append sb "UNIFY")
              (doseq [k ks]
                (.append sb (str " " k "=" (show* (subst* (get @th k) th)))))
              (.append sb "\n")))
          (recur (+ pos 2) (dec tcases) sb))))))

(-main)
