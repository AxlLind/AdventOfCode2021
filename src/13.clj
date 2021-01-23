(def input '(("Alice" -2 "Bob") ("Alice" -62 "Carol") ("Alice" 65 "David") ("Alice" 21 "Eric") ("Alice" -81 "Frank") ("Alice" -4 "George") ("Alice" -80 "Mallory") ("Bob" 93 "Alice") ("Bob" 19 "Carol") ("Bob" 5 "David") ("Bob" 49 "Eric") ("Bob" 68 "Frank") ("Bob" 23 "George") ("Bob" 29 "Mallory") ("Carol" -54 "Alice") ("Carol" -70 "Bob") ("Carol" -37 "David") ("Carol" -46 "Eric") ("Carol" 33 "Frank") ("Carol" -35 "George") ("Carol" 10 "Mallory") ("David" 43 "Alice") ("David" -96 "Bob") ("David" -53 "Carol") ("David" -30 "Eric") ("David" -12 "Frank") ("David" 75 "George") ("David" -20 "Mallory") ("Eric" 8 "Alice") ("Eric" -89 "Bob") ("Eric" -69 "Carol") ("Eric" -34 "David") ("Eric" 95 "Frank") ("Eric" 34 "George") ("Eric" -99 "Mallory") ("Frank" -97 "Alice") ("Frank" 6 "Bob") ("Frank" -9 "Carol") ("Frank" 56 "David") ("Frank" -17 "Eric") ("Frank" 18 "George") ("Frank" -56 "Mallory") ("George" 45 "Alice") ("George" 76 "Bob") ("George" 63 "Carol") ("George" 54 "David") ("George" 54 "Eric") ("George" 30 "Frank") ("George" 7 "Mallory") ("Mallory" 31 "Alice") ("Mallory" -32 "Bob") ("Mallory" 95 "Carol") ("Mallory" 91 "David") ("Mallory" -66 "Eric") ("Mallory" -75 "Frank") ("Mallory" -99 "George")))

(def graph (->> input (reduce (fn [m [a h b]] (assoc m [a b] h)) {})))

(def people (->> input (map first) set))

; https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s] (->> s (remove #{x}) permutations (map #(cons x %)))))
      [s])))

(defn happiness-change [[a b]]
  (+ (graph [a b]) (graph [b a])))

(defn calc-happiness-p2 [seats]
  (->> seats
    (partition 2 1)
    (map happiness-change)
    (reduce +)))

(defn calc-happiness-p1 [seats]
  (calc-happiness-p2 (conj seats (last seats))))

(defn max-happiness [f]
  (->> people permutations (map f) (apply max)))

(->> calc-happiness-p1 max-happiness (println "Part one:"))
(->> calc-happiness-p2 max-happiness (println "Part two:"))
