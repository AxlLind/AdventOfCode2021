(def input '[(:jio :a 19) (:inc :a) (:tpl :a) (:inc :a) (:tpl :a) (:inc :a) (:tpl :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:jmp 23) (:tpl :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:inc :a) (:tpl :a) (:inc :a) (:tpl :a) (:inc :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:inc :a) (:inc :a) (:tpl :a) (:tpl :a) (:inc :a) (:jio :a 8) (:inc :b) (:jie :a 4) (:tpl :a) (:inc :a) (:jmp 2) (:hlf :a) (:jmp -7)])

(defn run-inst [ip regs [code op1 op2]]
  (case code
    :hlf [(inc ip) (update regs op1 #(/ % 2))]
    :tpl [(inc ip) (update regs op1 #(* % 3))]
    :inc [(inc ip) (update regs op1 inc)]
    :jmp [(+ ip op1) regs]
    :jie [(+ ip (if (even? (regs op1)) op2 1)) regs]
    :jio [(+ ip (if (= 1   (regs op1)) op2 1)) regs]))

(defn run [insts [ip regs]]
  (let [i (get insts ip)]
  (if (not i)
    (regs :b)
    (recur insts (run-inst ip regs i)))))

(->> [0 {:a 0 :b 0}] (run input) (println "Part one:"))
(->> [0 {:a 1 :b 0}] (run input) (println "Part two:"))
