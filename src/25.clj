(def input '(3010 3019))

(defn walk-diagonal [r c pre]
  (if (and (= r (first input)) (= c (second input)))
    pre
  (let [new (mod (* pre 252533) 33554393)]
  (if (= r 1)
    (recur (inc c) 1 new)
    (recur (dec r) (inc c) new)))))

(println "Part one:" (walk-diagonal 1 1 20151125))
(println "Part two:" "🎄")
