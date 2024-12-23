(require '[clojure.math.combinatorics :as combo])

(def input '(1 3 5 11 13 17 19 23 29 31 37 41 43 47 53 59 67 71 73 79 83 89 97 101 103 107 109 113))
(def total-sum (reduce + input))

(defn valid-lists [target numbers len]
  (->> len
    (combo/combinations numbers)
    (filter #(= (reduce + %) target))))

(defn find-min [numbers parts]
  (->> (iterate inc 1)
    (map #(valid-lists (/ total-sum parts) numbers %))
    (filter not-empty)
    first
    (map #(reduce * %))
    (apply min)))

(->> 3 (find-min input) (println "Part one:"))
(->> 4 (find-min input) (println "Part two:"))
