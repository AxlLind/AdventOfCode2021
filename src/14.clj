(def input '((19 7 124) (3 15 28) (19 9 164) (19 9 158) (13 7 82) (25 6 145) (14 3 38) (3 16 37) (25 6 143)))

; how is this not in the stdlib?
(defn zip [coll1 coll2] (map vector coll1 coll2))

(defn flown [t [speed active inactive]]
  (let [seconds
    (+
      (*  (quot t (+ active inactive)) active)
      (min (rem t (+ active inactive)) active))]
  (* speed seconds)))

(defn give-point [max-d [points distance]]
  (+ points (if (= max-d distance) 1 0)))

(defn give-points [t points]
  (let [distances (map #(flown t %) input)]
    (->> distances
      (zip points)
      (map #(give-point (apply max distances) %))
      doall)))

(defn part2
  ([t points]
    (if (= t 2503)
      (apply max points)
      (recur (inc t) (give-points t points))))
  ([] (part2 1 (replicate (count input) 0))))

(->> input (map #(flown 2503 %)) (apply max) (println "Part one:"))
(println "Part two:" (part2))
