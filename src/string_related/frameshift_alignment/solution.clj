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
      dist (make-array Long/TYPE (inc n) nnodes)
      pk (make-array Byte/TYPE (inc n) nnodes)
      pu (make-array Long/TYPE (inc n) nnodes)
      pc (make-array Character/TYPE (inc n) nnodes)]
  (doseq [i (range (inc n)) v (range nnodes)] (aset-long dist i v INF))
  (aset-long dist 0 0 0)
  (doseq [i (range 1 (inc n))] (aset-long dist i 0 (* i dele)) (aset-byte pk i 0 1))
  (doseq [v (range 1 nnodes)]
    (loop [es (get incoming v []) best INF bu 0 bc \space]
      (if (empty? es)
        (do (aset-long dist 0 v best) (aset-byte pk 0 v 2) (aset-long pu 0 v bu) (aset pc 0 v bc))
        (let [[u c] (first es) cand (+ (aget dist 0 u) ins)]
          (if (< cand best)
            (recur (rest es) cand u c)
            (recur (rest es) best bu bc))))))
  (doseq [i (range 1 (inc n)) v (range 1 nnodes)]
    (let [es (get incoming v [])
          [best bk bu bc] (loop [es es best INF bk 0 bu 0 bc \space]
                            (if (empty? es) [best bk bu bc]
                                (let [[u c] (first es)
                                      cs (+ (aget dist (dec i) u) (if (= (nth s (dec i)) c) 0 sub))]
                                  (if (< cs best)
                                    (recur (rest es) cs 3 u c)
                                    (recur (rest es) best bk bu bc)))))
          [best bk] (let [cd (+ (aget dist (dec i) v) dele)]
                      (if (< cd best) [cd 1] [best bk]))
          [best bk bu bc] (loop [es es best best bk bk bu bu bc bc]
                            (if (empty? es) [best bk bu bc]
                                (let [[u c] (first es)
                                      ci (+ (aget dist i u) ins)]
                                  (if (< ci best)
                                    (recur (rest es) ci 2 u c)
                                    (recur (rest es) best bk bu bc)))))]
      (aset-long dist i v best)
      (aset-byte pk i v bk)
      (aset-long pu i v bu)
      (aset pc i v bc)))
  (loop [i n v t acc []]
    (if (and (zero? i) (zero? v))
      (print (str (aget dist n t) "\n" (apply str (reverse acc))))
      (case (aget pk i v)
        1 (recur (dec i) v acc)
        2 (recur i (aget pu i v) (conj acc (aget pc i v)))
        (recur (dec i) (aget pu i v) (conj acc (aget pc i v)))))))
