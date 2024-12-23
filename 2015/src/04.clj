(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def leading-5-zeros (BigInteger. "000001000000000000000000000000000" 16))
(def leading-6-zeros (BigInteger. "000000100000000000000000000000000" 16))

; from https://gist.github.com/jizhang/4325757
(defn md5 [s]
  (->> s
    .getBytes
    (.digest (MessageDigest/getInstance "MD5"))
    (BigInteger. 1)))

(defn find-hash [n]
  (->> (iterate inc 0)
    (filter #(->> % (str "ckczppom") md5 (> n)))
    first))

(->> leading-5-zeros find-hash (println "Part two:"))
(->> leading-6-zeros find-hash (println "Part one:"))
