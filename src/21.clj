(def input '(109 8 2))

(def weapons '((8  4 0) (10 5 0) (25 6 0) (40 7 0) (74 8 0)))
(def armor '((0 0 0) (13 0 1) (31 0 2) (53 0 3) (75 0 4) (102 0 5)))
(def rings '((0 0 0) (25 1 0) (50 2 0) (100 3 0) (20 0 1) (40 0 2) (80 0 3)))

(defn won-battle [dmg boss-hp boss-dmg]
  (or
    (<= boss-dmg 0)
    (<
      (quot boss-hp dmg)
      (quot 100 boss-dmg))))

(defn part1 [dmg boss-hp boss-dmg cost]
  (if (won-battle dmg boss-hp boss-dmg) cost 1000))

(defn part2 [dmg boss-hp boss-dmg cost]
  (if (won-battle dmg boss-hp boss-dmg) 0 cost))

(defn find-goal [goal cost-fn]
  (apply goal
    (for [[c1 d1 a1] weapons
          [c2 d2 a2] armor
          [c3 d3 a3] rings
          [c4 d4 a4] rings
          :when (not= [c3 d3 a3] [c4 d4 a4])]
    (let [[boss-hp d a] input
          dmg (- (+ d1 d2 d3 d4) a)
          boss-dmg (- d (+ a1 a2 a3 a4))]
      (cost-fn dmg boss-hp boss-dmg (+ c1 c2 c3 c4))))))

(->> part1 (find-goal min) (println "Part one:"))
(->> part2 (find-goal max) (println "Part two:"))
