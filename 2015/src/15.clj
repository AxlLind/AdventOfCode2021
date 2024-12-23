(def input '((3 0 0 -3 2) (-3 3 0 0 9) (-1 0 4 0 1) (0 0 -2 2 8)))

(defn zip [coll1 coll2] (map vector coll1 coll2))

(defn score [parts]
  (let [scores
    (->> parts
      (zip input)
      (map (fn [[ingredient spoons]] (map #(* spoons %) ingredient))))]
  (->> (range 4)
    (map (fn [i]
      (->> scores
        (map #(nth % i))
        (reduce +))))
    (map #(max 0 %))
    (reduce *))))

(defn score-p2 [parts]
  (->> parts
    (zip input)
    (map (fn [[ingredient spoons]] (* (nth ingredient 4) spoons)))
    (reduce +)
    (#(if (= % 500) (score parts) -1))))

(defn find-optimal [score-fn]
  (apply max
    (for [
      i (range 100)
      j (range (- 100 i))
      k (range (- 100 i j))]
    (score-fn [i j k (- 100 i j k)]))))

(->> score    find-optimal (println "Part one:"))
(->> score-p2 find-optimal (println "Part two:"))
