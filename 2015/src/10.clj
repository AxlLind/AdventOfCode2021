(def input "3113322113")

(defn expand
  ([res [c & s]]
    (if (not c)
      (->> res reverse (apply str))
      (if (not s)
        (recur (conj res "1" c) s)
        (let [[same rest] (split-with #(= c %) s)]
          (recur (conj res (inc (count same)) c) rest)))))
  ([s] (expand nil s)))

(defn repeated-expand [s n]
  (nth (iterate expand s) n))

(->> 40 (repeated-expand input) count (println "Part one:"))
(->> 50 (repeated-expand input) count (println "Part two:"))
