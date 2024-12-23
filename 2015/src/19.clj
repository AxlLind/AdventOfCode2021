(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.data.priority-map :refer [priority-map]])

(def input "Al => ThF\nAl => ThRnFAr\nB => BCa\nB => TiB\nB => TiRnFAr\nCa => CaCa\nCa => PB\nCa => PRnFAr\nCa => SiRnFYFAr\nCa => SiRnMgAr\nCa => SiTh\nF => CaF\nF => PMg\nF => SiAl\nH => CRnAlAr\nH => CRnFYFYFAr\nH => CRnFYMgAr\nH => CRnMgYFAr\nH => HCa\nH => NRnFYFAr\nH => NRnMgAr\nH => NTh\nH => OB\nH => ORnFAr\nMg => BF\nMg => TiMg\nN => CRnFAr\nN => HSi\nO => CRnFYFAr\nO => CRnMgAr\nO => HP\nO => NRnFAr\nO => OTi\nP => CaP\nP => PTi\nP => SiRnFAr\nSi => CaSi\nTh => ThCa\nTi => BP\nTi => TiTi\ne => HF\ne => NAl\ne => OMg")
(def molecule "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr")

(def formulas
  (->> input str/split-lines (map #(str/split % #" => "))))

(def reverse-formulas (map reverse formulas))

(defn molecules
  ([a b pre s res]
    (if (empty? s)
      res
      (let [res
        (if (str/starts-with? s a)
         (conj res (str pre b (subs s (count a))))
         res)]
      (recur a b (str pre (first s)) (subs s 1) res))))
  ([s [a b]] (molecules a b "" s #{})))

(defn next-molecules [fs s]
  (->> fs (map #(molecules s %)) (apply set/union)))

; a bfs using a priority queue, prioritized on smaller strings
(defn bfs [q]
  (let [[[s n] _] (peek q) q (pop q)]
  (if (= s "e")
    n
    (->> s
      (next-molecules reverse-formulas)
      (map vector (repeat (inc n)))
      (map (fn [[n s]] [[s n] (count s)]))
      (apply conj q)
      recur))))

(->> molecule (next-molecules formulas) count (println "Part one:"))
(->> (priority-map [molecule 0] 0) bfs (println "Part two:"))
