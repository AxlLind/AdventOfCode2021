(require '[clojure.string :as str])

(def lines (->> "inputs/08.in" slurp str/split-lines))

(defn chars-in-p1
  ([c [a & s]]
    (if (not a)
      c
      (let [n
        (cond
          (not= \\ a)      0
          (= (first s) \x) 3
          :else            1)]
      (recur (inc c) (drop n s)))))
  ([s] (chars-in-p1 -2 s)))

(defn chars-in-p2
  ([c [a & s]]
    (case a
      nil (+ 2 c)
      \"  (recur (inc c) s)
      \\  (recur (inc c) s)
      (recur c s)))
  ([s] (chars-in-p2 0 s)))

(->> lines (map chars-in-p1) (reduce +) (- (->> lines (map count) (reduce +))) println)
(->> lines (map chars-in-p2) (reduce +) println)
