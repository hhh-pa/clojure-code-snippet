;; Mini static linker (CS:APP ch 7: resolution + relocation).
(require '[clojure.string :as str])

(defn- link-resolve
  [objs text-base data-base]
  (let [defs (reduce (fn [m [fi o]]
                       (reduce (fn [m2 d]
                                 (let [base (nth (if (= (:sec d) "TEXT") text-base data-base) fi)]
                                   (update m2 (:name d) (fnil conj [])
                                           {:strong (:strong d) :file fi
                                            :addr (+ base (:off d))})))
                               m (:defs o)))
                     {} (map-indexed vector objs))
        names (sort (into (set (keys defs))
                          (mapcat (fn [o] (map :name (:relocs o))) objs)))]
    (loop [ns names syms (sorted-map)]
      (if (empty? ns)
        [syms nil]
        (let [name (first ns)
              ds (get defs name)]
          (cond
            (nil? ds) [nil (str "ERROR undefined " name)]
            (> (count (filter :strong ds)) 1) [nil (str "ERROR multiply-defined " name)]
            :else (let [st (filter :strong ds)
                        addr (if (= (count st) 1)
                               (:addr (first st))
                               (:addr (apply min-key :file ds)))]
                    (recur (rest ns) (assoc syms name addr)))))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          pos (atom 0)
          next-tok (fn [] (let [t (nth toks @pos)] (swap! pos inc) t))
          next-int (fn [] (Long/parseLong (next-tok)))
          m (next-int)
          objs (vec (for [_ (range m)]
                      (let [ts (next-int) ds (next-int) nd (next-int)
                            dd (vec (for [_ (range nd)]
                                      {:name (next-tok) :sec (next-tok)
                                       :off (next-int) :size (next-int)
                                       :strong (= (next-int) 1)}))
                            nr (next-int)
                            rr (vec (for [_ (range nr)]
                                      {:sec (next-tok) :off (next-int)
                                       :name (next-tok) :abs (= (next-tok) "ABS")}))]
                        {:text-size ts :data-size ds :defs dd :relocs rr})))
          text-base (vec (reductions + 0 (map :text-size objs)))
          text-size (reduce + (map :text-size objs))
          data-base (vec (map #(+ % text-size) (reductions + 0 (map :data-size objs))))
          data-size (- (reduce + text-size (map :data-size objs)) text-size)
          [syms err] (link-resolve objs text-base data-base)]
      (if err
        (print err)
        (let [out (atom [(str "TEXT 0 " text-size) (str "DATA " text-size " " data-size)])]
          (doseq [[name a] syms]
            (swap! out conj (str "SYM " name " " a)))
          (doseq [[fi o] (map-indexed vector objs)
                  r (:relocs o)]
            (let [base (nth (if (= (:sec r) "TEXT") text-base data-base) fi)
                  place (+ base (:off r))
                  v (if (:abs r)
                      (get syms (:name r))
                      (- (get syms (:name r)) (+ place 4)))]
              (swap! out conj (str "PATCH " fi " " (:sec r) " " (:off r) " " v))))
          (print (str/join "\n" @out)))))))
