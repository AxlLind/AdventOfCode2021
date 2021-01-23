(require '[clojure.string :as str])

; how is this not in the standard library?
(defn find-item [f coll]
  (->> coll (filter f) first))

(defn next-pw [pw]
  (case (last pw)
    \z (str (->> pw drop-last next-pw) \a)
    \h (str (->> pw drop-last (str/join "")) \j) ; skip i
    \n (str (->> pw drop-last (str/join "")) \p) ; skip o
    \k (str (->> pw drop-last (str/join "")) \m) ; skip l
    (str (->> pw drop-last (str/join "")) (->> pw last int inc char))))

(defn increasing-triple? [triple]
  (let [[a b c] (map int triple)]
    (= (inc a) b (dec c))))

(defn has-two-pairs? [[[a b] & pairs]]
  (and
    a
    (or
      (and (= a b) (some (fn [[c d]] (and (= c d) (not= a c))) (rest pairs)))
      (recur pairs))))

(defn valid-pw [pw]
  (and
    (->> pw (partition 3 1) (some increasing-triple?))
    (->> pw (partition 2 1) has-two-pairs?)))

(defn find-next-valid [start]
  (->> start (iterate next-pw) rest (find-item valid-pw)))

(let [[p1 p2] (->> "cqjxjnds" (iterate find-next-valid) (take 2))]
  (println "Part one:" p1)
  (println "Part two:" p2))
