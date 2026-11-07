;; Dijkstra shortest reach over many cases in pure clojure. A
;; binary heap over a flat transient vector (distance and vertex
;; alternating) gives O(E log V) time and O(V + E) space per case.
(require 'clojure.string)

(defn- parse-ints
  "Reads all signed integers from string s into a vector, else nil."
  [s]
  (let [toks (clojure.string/split s #"[\x00-\x20]+")
        nt (count toks)]
    (loop [i 0 out (transient [])]
      (if (= i nt)
        (persistent! out)
        (let [tok (nth toks i)]
          (if (= tok "")
            (recur (inc i) out)
            (if-let [n (when (not= (nth tok 0) \+) (parse-long tok))]
              (recur (inc i) (conj! out n))
              nil)))))))

(defn- report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn- sift-up
  "Moves the entry at e up the flat transient heap hp (distance
   and vertex alternating) until ordered."
  [hp e]
  (let [ed (nth hp (* 2 (long e)))
        ev (nth hp (+ (* 2 (long e)) 1))]
    (loop [h (long e) p (long (bit-shift-right (- (long e) 1) 1))]
      (if (<= h 0)
        (do (assoc! hp (* 2 h) ed)
            (assoc! hp (+ (* 2 h) 1) ev)
            hp)
        (if (>= (long ed) (long (nth hp (* 2 p))))
          (do (assoc! hp (* 2 h) ed)
              (assoc! hp (+ (* 2 h) 1) ev)
              hp)
          (do (assoc! hp (* 2 h) (nth hp (* 2 p)))
              (assoc! hp (+ (* 2 h) 1) (nth hp (+ (* 2 p) 1)))
              (recur p (bit-shift-right (- p 1) 1))))))))

(defn- sift-down
  "Moves the entry at 0 down the flat heap hp with bound n entries."
  [hp n]
  (let [ed (nth hp 0)
        ev (nth hp 1)]
    (loop [h (long 0) l (long 1) nn (long n)]
      (if (>= l nn)
        (do (assoc! hp (* 2 h) ed)
            (assoc! hp (+ (* 2 h) 1) ev)
            hp)
        (if (and (< (+ l 1) nn)
                 (< (long (nth hp (* 2 (+ l 1)))) (long (nth hp (* 2 l)))))
          (if (>= (long (nth hp (* 2 (+ l 1)))) (long ed))
            (do (assoc! hp (* 2 h) ed)
                (assoc! hp (+ (* 2 h) 1) ev)
                hp)
            (do (assoc! hp (* 2 h) (nth hp (* 2 (+ l 1))))
                (assoc! hp (+ (* 2 h) 1) (nth hp (+ (* 2 (+ l 1)) 1)))
                (recur (+ l 1) (+ (* 2 (+ l 1)) 1) nn)))
          (if (>= (long (nth hp (* 2 l))) (long ed))
            (do (assoc! hp (* 2 h) ed)
                (assoc! hp (+ (* 2 h) 1) ev)
                hp)
            (do (assoc! hp (* 2 h) (nth hp (* 2 l)))
                (assoc! hp (+ (* 2 h) 1) (nth hp (+ (* 2 l) 1)))
                (recur l (+ (* 2 l) 1) nn))))))))

(defn- add-exact
  [cd w maxl minl]
  (if (or (> cd maxl) (< cd minl))
    (+' cd w)
    (let [a (long cd) b (long w)]
      (if (or (and (>= b 0) (> a (- (long maxl) b)))
              (and (< b 0) (< a (- (long minl) b))))
        (+' cd w)
        (+ a b)))))

(defn- sift-up-packed
  [hp e entry]
  (loop [h e]
    (if (zero? h)
      (assoc! hp 0 entry)
      (let [p (bit-shift-right (dec h) 1)
            pe (nth hp p)]
        (if (<= pe entry)
          (assoc! hp h entry)
          (do (assoc! hp h pe)
              (recur p)))))))

(defn- sift-down-packed
  [hp n entry]
  (loop [h 0]
    (let [l (inc (bit-shift-left h 1))]
      (if (>= l n)
        (assoc! hp h entry)
        (let [r (inc l)
              le (nth hp l)]
          (if (< r n)
            (let [re (nth hp r)]
              (if (< re le)
                (if (<= entry re)
                  (assoc! hp h entry)
                  (do (assoc! hp h re)
                      (recur r)))
                (if (<= entry le)
                  (assoc! hp h entry)
                  (do (assoc! hp h le)
                      (recur l)))))
            (if (<= entry le)
              (assoc! hp h entry)
              (do (assoc! hp h le)
                  (recur l)))))))))

(defn- dijkstra-line
  [nums p]
  (let [c (count nums)]
    (when (>= (- c p) 2)
      (let [n (nth nums p)
            m (nth nums (inc p))]
        (when (and (>= n 1) (>= m 0) (>= (- c p) (+' 3 (*' m 3))))
          (let [head (transient (vec (repeat n -1)))
                edges (transient [])
                wt-v (transient [])
                state (loop [i 0 fast? (< n 1048576) max-w 0]
                        (if (= i m)
                          (if (and fast? (< (*' n max-w) 8796093022208)) 1 0)
                          (let [base (+ p 2 (* i 3))
                                x (nth nums base)
                                y (nth nums (inc base))
                                r (nth nums (+ base 2))]
                            (if (or (< x 1) (> x n) (< y 1) (> y n))
                              -1
                              (let [u (dec x)
                                    v (dec y)
                                    e1 (* i 2)
                                    e2 (inc e1)
                                    hu (nth head u)
                                    hv (nth head v)]
                                (conj! edges (bit-or v (bit-shift-left (inc hu) 24)))
                                (conj! wt-v r)
                                (assoc! head u e1)
                                (conj! edges (bit-or u (bit-shift-left (inc hv) 24)))
                                (conj! wt-v r)
                                (assoc! head v e2)
                                (recur (inc i)
                                       (and fast? (>= r 0) (< r 1000000000))
                                       (if (> r max-w) r max-w)))))))]
            (when (>= state 0)
              (let [src (nth nums (+ p 2 (* m 3)))]
                (when (and (>= src 1) (<= src n))
                  (let [s (dec src)
                        dist (assoc! (transient (vec (repeat n -1))) s 0)]
                    (if (= state 1)
                      (let [hp (transient [s])]
                        (loop [sz 1]
                          (when (pos? sz)
                            (let [top (nth hp 0)
                                  cd (bit-shift-right top 20)
                                  v (bit-and top 1048575)
                                  sz0 (dec sz)
                                  _ (when (pos? sz0)
                                      (sift-down-packed hp sz0 (nth hp sz0)))]
                              (if (not= cd (nth dist v))
                                (recur sz0)
                                (recur (loop [e (nth head v) sz sz0]
                                         (if (= e -1)
                                           sz
                                           (let [ev (nth edges e)
                                                 to (bit-and ev 16777215)
                                                 ne (dec (bit-shift-right ev 24))
                                                 ndv (+ cd (nth wt-v e))
                                                 old (nth dist to)]
                                             (if (or (= old -1) (< ndv old))
                                               (let [nentry (bit-or (bit-shift-left ndv 20) to)]
                                                 (assoc! dist to ndv)
                                                 (if (< sz (count hp))
                                                   (assoc! hp sz nentry)
                                                   (conj! hp nentry))
                                                 (sift-up-packed hp sz nentry)
                                                 (recur ne (inc sz)))
                                               (recur ne sz)))))))))))
                      (let [maxl 9223372036854775807
                            minl -9223372036854775808
                            hp (conj! (conj! (transient []) 0) s)]
                        (loop [sz (long 1)]
                          (when (pos? sz)
                            (let [cd (nth hp 0)
                                  v (nth hp 1)
                                  _ (when (> sz 1)
                                      (let [ld (nth hp (* (- sz 1) 2))
                                            lv (nth hp (+ (* (- sz 1) 2) 1))]
                                        (assoc! hp 0 ld)
                                        (assoc! hp 1 lv)
                                        (sift-down hp (- sz 1))))]
                              (if (not= cd (nth dist v))
                                (recur (- sz 1))
                                (let [sz1 (loop [e (nth head v)
                                                 sz (long (- sz 1))]
                                            (if (= e -1)
                                              sz
                                              (let [ev (nth edges e)
                                                    to (bit-and ev 16777215)
                                                    ne (dec (bit-shift-right ev 24))
                                                    w (nth wt-v e)
                                                    old (nth dist to)
                                                    ndv (add-exact cd w maxl minl)]
                                                (if (or (= old -1) (< ndv old))
                                                  (do (assoc! dist to ndv)
                                                      (if (< (* 2 sz) (count hp))
                                                        (do (assoc! hp (* 2 sz) ndv)
                                                            (assoc! hp (+ (* 2 sz) 1) to))
                                                        (do (conj! hp ndv)
                                                            (conj! hp to)))
                                                      (sift-up hp sz)
                                                      (recur ne (+ sz 1)))
                                                  (recur ne sz)))))]
                                  (recur sz1))))))))
                    (let [out (loop [i 0 acc (transient [])]
                                (if (= i n)
                                  (persistent! acc)
                                  (if (= i s)
                                    (recur (inc i) acc)
                                    (recur (inc i) (conj! acc (str (nth dist i) " "))))))]
                      [(+ p 3 (* m 3))
                       (str (apply str out) "\n")])))))))))))

(defn- solve
  [nums]
  (let [t (nth nums 0)]
    (if (< t 0)
      (report-error "bad input: case count must not be negative")
      (if (zero? t)
        nil
        (let [lines (loop [tc 0 p 1 acc (transient [])]
                      (if (>= tc t)
                        (persistent! acc)
                        (let [one (dijkstra-line nums p)]
                          (if (nil? one)
                            nil
                            (recur (inc tc) (first one)
                                   (conj! acc (second one)))))))]
          (if (nil? lines)
            (report-error "bad input: case shape wrong or vertex out of range")
            (do (print (apply str lines)) nil)))))))

(defn -main
  []
  (let [nums (parse-ints (slurp *in*))]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main)
