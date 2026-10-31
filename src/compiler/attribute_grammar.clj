
;; attribute_grammar: signed binary numbers via inherited and
;; synthesized attributes. The sign is inherited down the tree while
;; bit values synthesize upward through the left-recursive backbone.
(ns attribute-grammar
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn synth [node]
  (if (nil? (:l node))
    (:bit node)
    (+ (* (synth (:l node)) 2) (:bit node))))

(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (loop [pos 1 tcases (parse-token (first toks)) sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [sg (nth toks pos)
              bs (nth toks (inc pos))
              root (loop [i 0 r nil]
                     (if (= i (count bs))
                       r
                       (recur (inc i) {:l r :bit (if (= (.charAt bs i) \1) 1 0)})))
              v (synth root)]
          (.append sb (str "ATTR " (if (= sg "-") (- v) v) "\n"))
          (recur (+ pos 2) (dec tcases) sb))))))

(-main)
