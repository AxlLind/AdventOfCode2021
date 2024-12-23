(require '[clojure.data.priority-map :refer [priority-map]])

(def boss-start-hp 55)
(def boss-dmg 8)

(defn non-pos? [n] (<= n 0))

(defn apply-effects [debuf [hp boss-hp mana shield poison recharge spent]]
  [(- hp debuf)
   (- boss-hp (if (pos-int? poison) 3 0))
   (+ mana    (if (pos-int? recharge) 101 0))
   (dec shield)
   (dec poison)
   (dec recharge)
   spent])

(defn boss-move [state]
  (let [[hp boss-hp mana shield poison recharge spent] (apply-effects 0 state)]
  (if (non-pos? boss-hp)
    [spent]
    [(- hp (- boss-dmg (if (pos-int? shield) 7 0))) boss-hp mana shield poison recharge spent])))

(defn next-moves [dmg state]
  (let [
    [hp boss-hp mana shield poison recharge spent] (apply-effects dmg state)
    moves [
      [hp (- boss-hp 4) (- mana 53) shield poison recharge (+ spent 53)]
      [(+ hp 2) (- boss-hp 2) (- mana 73) shield poison recharge (+ spent 73)]
      (if (non-pos? shield)   [hp boss-hp (- mana 113) 6 poison recharge (+ spent 113)])
      (if (non-pos? poison)   [hp boss-hp (- mana 173) shield 6 recharge (+ spent 173)])
      (if (non-pos? recharge) [hp boss-hp (- mana 229) shield poison 5   (+ spent 229)])]]
    (->> moves
      (filter boolean)           ; remove effects that hadn't run out
      (filter #(<= 0 (first %))) ; remove those where I died from the debuf
      (filter #(<= 0 (nth % 2))) ; remove spells I did not have enough mana for
      (map boss-move)            ; make the boss move
      (filter #(<= 0 (first %))) ; remove those where I died from the boss
      )))

(defn bfs [dmg q]
  (let [[state _] (peek q) q (pop q)]
    (if (= 1 (count state))
      (first state)
    (->> state
      (next-moves dmg)
      (map (fn [state] [state (last state)]))
      (apply conj q)
      (recur dmg)))))

(->> (priority-map [50 boss-start-hp 500 0 0 0 0] 0) (bfs 0) (println "Part one:"))
(->> (priority-map [49 boss-start-hp 500 0 0 0 0] 0) (bfs 1) (println "Part one:"))
