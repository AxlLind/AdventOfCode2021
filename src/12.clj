(require '[cheshire.core :as cc])

(def json (->> "inputs/12.in" slurp cc/parse-string))

(defn sum-json-p1 [o]
  (cond
    (map?     o) (->> o vals (map sum-json-p1) (reduce +))
    (vector?  o) (->> o      (map sum-json-p1) (reduce +))
    (integer? o) o
    :else        0))

(defn sum-json-p2 [o]
  (cond
    (map? o)
      (if (->> o vals (some #(= "red" %)))
        0
        (->> o vals (map sum-json-p2) (reduce +)))
    (vector?  o) (->> o (map sum-json-p2) (reduce +))
    (integer? o) o
    :else        0))

(->> json sum-json-p1 (println "Part one:"))
(->> json sum-json-p2 (println "Part two:"))
