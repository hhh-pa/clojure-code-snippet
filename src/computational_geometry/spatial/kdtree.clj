(ns computational-geometry.spatial.kdtree
  (:require [clojure.string :as str]))

(defn read-tokens []
  (let [content (slurp *in*)]
    (remove str/blank? (str/split content #"\s+"))))

(defn build-kdtree [pts]
  (let [tree (java.util.ArrayList.)]
    (letfn [(build [pts-vec depth]
              (let [n (count pts-vec)]
                (if (zero? n)
                  -1
                  (let [axis (rem depth 2)
                        sorted-pts (vec (if (zero? axis)
                                          (sort-by (fn [[x _]] (double x)) pts-vec)
                                          (sort-by (fn [[_ y]] (double y)) pts-vec)))
                        mid (quot n 2)
                        mid-pt (nth sorted-pts mid)
                        node-idx (.size tree)]
                    (.add tree {:pt mid-pt :left -1 :right -1 :axis axis})
                    (let [left-child (build (subvec sorted-pts 0 mid) (inc depth))
                          right-child (build (subvec sorted-pts (inc mid) n) (inc depth))]
                      (.set tree node-idx (assoc (.get tree node-idx)
                                                 :left left-child
                                                 :right right-child))
                      node-idx)))))]
      (build pts 0)
      tree)))

(defn count-in-range [^java.util.ArrayList tree xmin ymin xmax ymax]
  (let [count-ref (int-array 1)]
    (letfn [(query [u]
              (when-not (= u -1)
                (let [{:keys [pt left right axis]} (.get tree u)
                      [^double x ^double y] pt]
                  (when (and (>= x xmin) (<= x xmax) (>= y ymin) (<= y ymax))
                    (aset count-ref 0 (inc (aget count-ref 0))))
                  (if (zero? axis)
                    (do
                      (when (<= xmin x) (query left))
                      (when (>= xmax x) (query right)))
                    (do
                      (when (<= ymin y) (query left))
                      (when (>= ymax y) (query right)))))))]
      (query 0)
      (aget count-ref 0))))

(defn find-nearest [^java.util.ArrayList tree ^double qx ^double qy]
  (let [best-dsq (double-array [1e18])
        best-pt (object-array [[0.0 0.0]])]
    (letfn [(query [u]
              (when-not (= u -1)
                (let [{:keys [pt left right axis]} (.get tree u)
                      [^double x ^double y] pt
                      dsq (+ (* (- x qx) (- x qx)) (* (- y qy) (- y qy)))]
                  (when (< dsq (aget best-dsq 0))
                    (aset best-dsq 0 dsq)
                    (aset best-pt 0 pt))
                  (let [axis-diff (if (zero? axis) (- qx x) (- qy y))
                        first-child (if (<= axis-diff 0.0) left right)
                        second-child (if (<= axis-diff 0.0) right left)]
                    (query first-child)
                    (when (< (* axis-diff axis-diff) (aget best-dsq 0))
                      (query second-child))))))]
      (query 0)
      [(aget best-pt 0) (Math/sqrt (aget best-dsq 0))])))

(defn -main [& _args]
  (let [tokens (read-tokens)]
    (when (seq tokens)
      (let [n (Long/parseLong (first tokens))
            pts-tokens (take (* 2 n) (rest tokens))
            tokens (drop (inc (* 2 n)) tokens)
            pts (vec (for [[x y] (partition 2 pts-tokens)]
                       [(Double/parseDouble x) (Double/parseDouble y)]))
            tree (build-kdtree pts)]
        (when (seq tokens)
          (let [num-queries (Long/parseLong (first tokens))
                tokens (rest tokens)]
            (loop [q 0
                   rem-tokens tokens]
              (when (and (< q num-queries) (seq rem-tokens))
                (let [q-type (first rem-tokens)]
                  (if (= q-type "RANGE")
                    (let [x1 (Double/parseDouble (nth rem-tokens 1))
                          y1 (Double/parseDouble (nth rem-tokens 2))
                          x2 (Double/parseDouble (nth rem-tokens 3))
                          y2 (Double/parseDouble (nth rem-tokens 4))]
                      (println (count-in-range tree x1 y1 x2 y2))
                      (recur (inc q) (drop 5 rem-tokens)))
                    (let [qx (Double/parseDouble (nth rem-tokens 1))
                          qy (Double/parseDouble (nth rem-tokens 2))
                          [[px py] d] (find-nearest tree qx qy)]
                      (println (String/format java.util.Locale/US "%.2f %.2f %.2f" (to-array [(double px) (double py) (double d)])))
                      (recur (inc q) (drop 3 rem-tokens)))))))))))))

(-main)
