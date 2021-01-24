(def input '(43 3 4 10 21 44 4 6 47 41 34 17 17 44 36 31 46 9 27 38))

(defn powerset [base-set]
  (->> base-set
       (reduce
         (fn [xs x]
           (concat xs
                   (map #(cons x %) xs)))
         [()])))

(let [valid (->> input powerset (filter #(= 150 (reduce + %))))]
  (->> valid count (println "Part one:"))
  (->> valid (map count) frequencies (apply min-key first) second (println "Part two:")))
