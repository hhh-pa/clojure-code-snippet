;; Genetic algorithms for N-queens (AIMA 4.1.4).
(require '[clojure.string :as str])

(def ^:const MOD (bit-shift-left 1 31))

(defn- gen-draw
  [rng upper]
  (let [nr (mod (+ (* 1103515245 @rng) 12345) MOD)]
    (reset! rng nr)
    (mod nr upper)))

(defn- gen-fitness
  [s]
  (let [n (count s)]
    (- (quot (* n (dec n)) 2)
       (count (for [i (range n) j (range (inc i) n)
                    :when (or (= (nth s i) (nth s j))
                              (= (Math/abs (- (nth s i) (nth s j))) (- j i)))]
                1)))))

(defn- gen-select
  [pop fit total rng]
  (if (zero? total)
    (vec (nth pop (gen-draw rng (count pop))))
    (let [w (gen-draw rng total)]
      (loop [i 0 acc 0]
        (let [na (+ acc (nth fit i))]
          (if (< w na)
            (vec (nth pop i))
            (recur (inc i) na)))))))

(defn- gen-child
  [p1 p2 c mnum mden rng]
  (mapv (fn [j g1 g2]
          (let [g (if (< j c) g1 g2)]
            (if (< (gen-draw rng mden) mnum)
              (inc (gen-draw rng (count p1)))
              g)))
        (range (count p1)) p1 p2))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          p (Long/parseLong (nth toks 1))
          g (Long/parseLong (nth toks 2))
          mnum (Long/parseLong (nth toks 3))
          mden (Long/parseLong (nth toks 4))
          rng (atom (Long/parseLong (nth toks 5)))
          pop (mapv (fn [i] (mapv #(Character/digit ^char % 10) (nth toks (+ 6 i))))
                    (range p))]
      (loop [gen 0 pop pop]
        (if (>= gen g)
          (let [fit (mapv gen-fitness pop)
                bi (reduce (fn [b i] (if (> (nth fit i) (nth fit b)) i b))
                           0 (range 1 (count pop)))
                strs (mapv #(apply str %) pop)]
            (print (str (str/join "\n" strs) "\nBEST " (nth strs bi) " " (nth fit bi))))
          (let [fit (mapv gen-fitness pop)
                total (reduce + fit)
                nxt (vec (mapcat (fn [_]
                                   (let [a (gen-select pop fit total rng)
                                         b (gen-select pop fit total rng)
                                         c (inc (gen-draw rng (dec n)))]
                                     [(gen-child a b c mnum mden rng)
                                      (gen-child b a c mnum mden rng)]))
                                 (range (quot p 2))))]
            (recur (inc gen) nxt)))))))
