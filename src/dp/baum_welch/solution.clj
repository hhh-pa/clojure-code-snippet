;; Baum-Welch HMM training (AIMA 21.3.3, scaled forward-backward).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- bw-forward
  [pi a b obs]
  (let [s (count pi)
        t (count obs)
        al0 (mapv (fn [i] (* (nth pi i) (nth (nth b i) (nth obs 0)))) (range s))
        c0 (reduce + al0)
        al0 (mapv #(/ % c0) al0)]
    (loop [tt 1 al [al0] c [c0]]
      (if (>= tt t)
        [al c]
        (let [row (mapv (fn [j]
                          (* (nth (nth b j) (nth obs tt))
                             (reduce + (map (fn [i] (* (nth (nth al (dec tt)) i)
                                                         (nth (nth a i) j)))
                                            (range s)))))
                        (range s))
              ct (reduce + row)]
          (recur (inc tt) (conj al (mapv #(/ % ct) row)) (conj c ct)))))))

(defn- bw-backward
  [a b obs c]
  (let [s (count a)
        t (count obs)]
    (loop [tt (- t 2) be (assoc (vec (repeat t nil)) (dec t) (vec (repeat s 1.0)))]
      (if (neg? tt)
        be
        (recur (dec tt)
               (assoc be tt
                      (mapv (fn [i]
                              (/ (reduce + (map (fn [j]
                                                  (* (* (nth (nth a i) j)
                                                        (nth (nth b j) (nth obs (inc tt))))
                                                     (nth (nth be (inc tt)) j)))
                                                (range s)))
                                 (nth c (inc tt))))
                            (range s))))))))

(defn- bw-gammas
  [al be]
  (mapv (fn [ar br]
          (let [tot (reduce + (map * ar br))]
            (mapv (fn [x y] (/ (* x y) tot)) ar br)))
        al be))

(defn- bw-xi-prod
  [al be a b obs tt i j]
  (* (* (* (nth (nth al tt) i) (nth (nth a i) j))
        (nth (nth b j) (nth obs (inc tt))))
     (nth (nth be (inc tt)) j)))

(defn- bw-xis
  [al be a b obs]
  (let [s (count a)
        t (count obs)]
    (mapv (fn [tt]
            (let [tot (reduce + (for [i (range s) j (range s)]
                                  (bw-xi-prod al be a b obs tt i j)))]
              (mapv (fn [i]
                      (mapv (fn [j]
                              (/ (bw-xi-prod al be a b obs tt i j) tot))
                            (range s)))
                    (range s))))
          (range (dec t)))))

(defn- bw-accumulate
  [ac s obs gam xi]
  (let [t (count obs)
        ac (update ac :pi-num (fn [v] (mapv + v (nth gam 0))))
        ac (reduce (fn [ac tt]
                     (reduce (fn [ac2 i]
                               (let [m (update-in ac2 [:a-den i] + (nth (nth gam tt) i))]
                                 (reduce (fn [mm j]
                                           (update-in mm [:a-num i j]
                                                      + (nth (nth (nth xi tt) i) j)))
                                         m (range s))))
                             ac (range s)))
                   ac (range (dec t)))]
    (reduce (fn [ac tt]
              (reduce (fn [ac2 j]
                        (-> ac2
                            (update-in [:b-den j] + (nth (nth gam tt) j))
                            (update-in [:b-num j (nth obs tt)] + (nth (nth gam tt) j))))
                      ac (range s)))
            ac (range t))))

(defn- bw-train
  [pi a b seqs iters]
  (let [s (count pi)
        o (count (first b))]
    (loop [it 0 pi pi a a b b]
      (if (>= it iters)
        [pi a b]
        (let [acc (reduce (fn [ac obs]
                            (let [[al c] (bw-forward pi a b obs)
                                  be (bw-backward a b obs c)
                                  gam (bw-gammas al be)
                                  xi (bw-xis al be a b obs)]
                              (bw-accumulate ac s obs gam xi)))
                          {:pi-num (vec (repeat s 0.0))
                           :a-num (vec (repeat s (vec (repeat s 0.0))))
                           :a-den (vec (repeat s 0.0))
                           :b-num (vec (repeat s (vec (repeat o 0.0))))
                           :b-den (vec (repeat s 0.0))}
                          seqs)
              nl (count seqs)
              pi (mapv #(/ % nl) (:pi-num acc))
              a (mapv (fn [i row]
                        (if (pos? (nth (:a-den acc) i))
                          (mapv #(/ % (nth (:a-den acc) i)) row)
                          (nth a i)))
                      (range s) (:a-num acc))
              b (mapv (fn [j row]
                        (if (pos? (nth (:b-den acc) j))
                          (mapv #(/ % (nth (:b-den acc) j)) row)
                          (nth b j)))
                      (range s) (:b-num acc))]
          (recur (inc it) pi a b))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          next-int (fn [] (Long/parseLong (next-tok)))
          next-dbl (fn [] (Double/parseDouble (next-tok)))
          s (next-int) o (next-int) nl (next-int) iters (next-int)
          pi (vec (repeatedly s next-dbl))
          a (vec (for [_ (range s)] (vec (repeatedly s next-dbl))))
          b (vec (for [_ (range s)] (vec (repeatedly o next-dbl))))
          seqs (vec (for [_ (range nl)]
                      (let [t (next-int)] (vec (repeatedly t next-int)))))
          [pi a b] (bw-train pi a b seqs iters)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          row (fn [r] (str/join " " (map fmt r)))]
      (print (str/join "\n" (concat [(row pi)] (map row a) (map row b)))))))
