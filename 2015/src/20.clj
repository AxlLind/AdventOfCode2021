(def input 34000000)

(defn divisors
  ([ds i n]
    (if (> (* i i) n)
      ds
      (if (not= 0 (rem n i))
        (recur ds (inc i) n)
        (recur (conj ds i (quot n i)) (inc i) n))))
  ([n] (divisors #{} 1 n)))

(defn presents-p1 [n]
  (->> n divisors (reduce +) (* 10)))

(defn presents-p2 [n]
  (->> n divisors (filter #(<= (quot n %) 50)) (reduce +) (* 11)))

(defn find-first-house [target f]
  (->> (iterate inc 1) (filter #(> (f %) target)) first))

(->> presents-p1 (find-first-house input) (println "Part one:"))
(->> presents-p2 (find-first-house input) (println "Part two:"))
