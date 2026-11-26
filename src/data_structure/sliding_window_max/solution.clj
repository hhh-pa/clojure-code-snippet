;; https://leetcode.com/problems/sliding-window-maximum/
;; Sliding window maximum (monotonic deque, O(n)).
(require '[clojure.string :as str])
(import '[java.util ArrayDeque])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          k (nth toks 1)
          a (subvec toks 2 (+ 2 n))
          dq (ArrayDeque.)]
      (loop [i 0 out []]
        (if (>= i n)
          (print (str/join " " out))
          (let [x (nth a i)]
            (while (and (not (.isEmpty dq)) (<= (nth a (.peekLast dq)) x))
              (.pollLast dq))
            (.addLast dq i)
            (while (<= (.peekFirst dq) (- i k))
              (.pollFirst dq))
            (recur (inc i) (if (>= (inc i) k) (conj out (nth a (.peekFirst dq))) out))))))))
