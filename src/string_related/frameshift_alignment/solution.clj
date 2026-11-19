;; Frameshift-aware DNA/protein alignment (string-to-network matching).

(def CODONS
  {\F ["TTT" "TTC"] \L ["TTA" "TTG" "CTT" "CTC" "CTA" "CTG"]
   \I ["ATT" "ATC" "ATA"] \M ["ATG"]
   \V ["GTT" "GTC" "GTA" "GTG"] \S ["TCT" "TCC" "TCA" "TCG" "AGT" "AGC"]
   \P ["CCT" "CCC" "CCA" "CCG"] \T ["ACT" "ACC" "ACA" "ACG"]
   \A ["GCT" "GCC" "GCA" "GCG"] \Y ["TAT" "TAC"]
   \H ["CAT" "CAC"] \Q ["CAA" "CAG"] \N ["AAT" "AAC"]
   \K ["AAA" "AAG"] \D ["GAT" "GAC"] \E ["GAA" "GAG"]
   \C ["TGT" "TGC"] \W ["TGG"]
   \R ["CGT" "CGC" "CGA" "CGG" "AGA" "AGG"]
   \G ["GGT" "GGC" "GGA" "GGG"]})

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+"))
      sub (Long/parseLong (nth toks 1)) ins (Long/parseLong (nth toks 2))
      dele (Long/parseLong (nth toks 3)) s (nth toks 4) prot (nth toks 5)
      n (count s)
      net (loop [aas (seq prot) nid 0 entry 0 edges []]
            (if (empty? aas) [edges (inc nid) entry]
                (let [nex (inc nid)
                      [edges nid] (loop [cods (sort (get CODONS (first aas))) edges edges nid nex]
                                    (if (empty? cods) [edges nid]
                                        (let [[edges u nid] (loop [chs (seq (first cods)) u entry edges edges nid nid]
                                                              (if (empty? chs) [edges u nid]
                                                                  (let [v (inc nid)]
                                                                    (recur (rest chs) v (conj edges [u v (first chs)]) v))))
                                              last u]
                                          (recur (rest cods)
                                                 (mapv (fn [[a b c]] [(if (= a last) nex a) (if (= b last) nex b) c]) edges)
                                                 (if (= last nex) nid (dec nid))))))]
                  (recur (rest aas) nid nex edges))))
      edges (nth net 0) nnodes (nth net 1) t (nth net 2)
      incoming (reduce (fn [m [u v c]] (update m v (fnil conj []) [u c])) {} edges)
      incoming (into {} (map (fn [[v es]] [v (vec (sort-by (fn [[u c]] [c u]) es))]) incoming))
      INF 4000000000000000000
      dist0 (vec (repeat (inc n) (vec (repeat nnodes INF))))
      pk0 (vec (repeat (inc n) (vec (repeat nnodes 0))))
      pu0 (vec (repeat (inc n) (vec (repeat nnodes 0))))
      pc0 (vec (repeat (inc n) (vec (repeat nnodes \space))))
      dist (assoc-in dist0 [0 0] 0)
      [dist pk] (reduce (fn [[dist pk] i]
                          [(assoc-in dist [i 0] (* i dele)) (assoc-in pk [i 0] 1)])
                        [dist pk0] (range 1 (inc n)))
      [dist pk pu pc] (reduce (fn [[dist pk pu pc] v]
                                (loop [es (get incoming v []) best INF bu 0 bc \space]
                                  (if (empty? es)
                                    [(assoc-in dist [0 v] best) (assoc-in pk [0 v] 2)
                                     (assoc-in pu [0 v] bu) (assoc-in pc [0 v] bc)]
                                    (let [[u c] (first es) cand (+ (get-in dist [0 u]) ins)]
                                      (if (< cand best)
                                        (recur (rest es) cand u c)
                                        (recur (rest es) best bu bc))))))
                              [dist pk pu0 pc0] (range 1 nnodes))
      [dist pk pu pc] (reduce (fn [[dist pk pu pc] i]
                                (reduce (fn [[dist pk pu pc] v]
                                          (let [es (get incoming v [])
                                                [best bk bu bc] (loop [es es best INF bk 0 bu 0 bc \space]
                                                                  (if (empty? es) [best bk bu bc]
                                                                      (let [[u c] (first es)
                                                                            cs (+ (get-in dist [(dec i) u])
                                                                                   (if (= (nth s (dec i)) c) 0 sub))]
                                                                        (if (< cs best)
                                                                          (recur (rest es) cs 3 u c)
                                                                          (recur (rest es) best bk bu bc)))))
                                                [best bk] (let [cd (+ (get-in dist [(dec i) v]) dele)]
                                                            (if (< cd best) [cd 1] [best bk]))
                                                [best bk bu bc] (loop [es es best best bk bk bu bu bc bc]
                                                                  (if (empty? es) [best bk bu bc]
                                                                      (let [[u c] (first es)
                                                                            ci (+ (get-in dist [i u]) ins)]
                                                                        (if (< ci best)
                                                                          (recur (rest es) ci 2 u c)
                                                                          (recur (rest es) best bk bu bc)))))]
                                            [(assoc-in dist [i v] best) (assoc-in pk [i v] bk)
                                             (assoc-in pu [i v] bu) (assoc-in pc [i v] bc)]))
                                        [dist pk pu pc] (range 1 nnodes)))
                              [dist pk pu pc] (range 1 (inc n)))]
  (loop [i n v t acc []]
    (if (and (zero? i) (zero? v))
      (print (str (get-in dist [n t]) "\n" (apply str (reverse acc))))
      (case (get-in pk [i v])
        1 (recur (dec i) v acc)
        2 (recur i (get-in pu [i v]) (conj acc (get-in pc [i v])))
        (recur (dec i) (get-in pu [i v]) (conj acc (get-in pc [i v])))))))
