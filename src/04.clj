(import 'java.security.MessageDigest
        'java.math.BigInteger)

; from https://gist.github.com/jizhang/4325757
(defn md5 [s]
  (->> s
    .getBytes
    (.digest (MessageDigest/getInstance "MD5"))
    (BigInteger. 1)
    (format "%032x")))

(defn leading-zeroes? [n s]
  (->> s
    (str "ckczppom")
    md5
    (take-while #(= \0 %))
    count
    (= n)))

(defn find-leading [n]
  (->> (iterate inc 0) (filter #(leading-zeroes? n %)) first))

(->> 5 find-leading (println "Part one:"))
(->> 6 find-leading (println "Part two:"))
