;; termination_detection: Dijkstra-Scholten termination detection for
;; diffusing computations. Engaged nodes send basics (deficit++); receipt by
;; an idle node engages it as a child of the sender; receipt by an engaged
;; node returns an immediate signal. An idle node with zero deficit signals
;; its parent (cascading); origin 0, idle with no children and no deficit,
;; announces termination. Batch 6 (T340). Input: n E, SEND/IDLE events.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn settle [st x]
  (loop [s st x x]
    (if (pos? (nth (:D s) x)) [s nil]
      (if (zero? x)
        (if (zero? (nth (:C s) 0)) [s :term] [s nil])
        (let [p (nth (:par s) x)
              s2 (-> s (update-in [:C p] dec) (update-in [:D p] dec) (update :sig inc))]
          (if (nth (:eng s2) p) [s2 nil] (recur s2 p)))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (Long/parseLong (nth toks 0)) E (Long/parseLong (nth toks 1))]
        (loop [k 0 p 2 eng (assoc (vec (repeat n false)) 0 true) par (vec (repeat n -1))
               C (vec (repeat n 0)) D (vec (repeat n 0)) sig 0 ign 0 term -1]
          (if (or (>= k E) (>= term 0))
            (do (println (str "TERMINATED: " (if (>= term 0) term "NEVER")))
                (println (str "SIGNALS: " sig)) (println (str "IGNORED: " ign)))
            (let [op (nth toks p)]
              (if (= op "SEND")
                (let [u (Long/parseLong (nth toks (+ p 1))) v (Long/parseLong (nth toks (+ p 2)))]
                  (if-not (nth eng u)
                    (recur (inc k) (+ p 3) eng par C D sig (inc ign) term)
                    (let [D2 (update D u inc)]
                      (if-not (nth eng v)
                        (recur (inc k) (+ p 3) (assoc eng v true) (assoc par v u)
                               (update C u inc) D2 sig ign term)
                        (recur (inc k) (+ p 3) eng par C (update D2 u dec) (inc sig) ign term)))))
                (let [x0 (Long/parseLong (nth toks (+ p 1)))]
                  (if-not (nth eng x0)
                    (recur (inc k) (+ p 2) eng par C D sig ign term)
                    (let [[s2 t?] (settle {:eng (assoc eng x0 false) :par par :C C :D D :sig sig} x0)]
                      (recur (inc k) (+ p 2) (:eng s2) (:par s2) (:C s2) (:D s2) (:sig s2) ign
                             (if t? (inc k) term)))))))))))))
(-main (slurp *in*))
