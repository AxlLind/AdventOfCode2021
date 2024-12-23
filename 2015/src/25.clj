(def input '(3010 3019))

(defn walk-diagonal [r c v]
  (if (= [r c] input)
    v
    (let [[r c] (if (= r 1) [(inc c) 1] [(dec r) (inc c)])]
      (recur r c (mod (* v 252533) 33554393)))))

(println "Part one:" (walk-diagonal 1 1 20151125))
(println "Part two:" "🎄")
