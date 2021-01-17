(import 'java.security.MessageDigest
        'java.math.BigInteger)
(defn md5 [s]
  (->> s
    .getBytes
    (.digest (MessageDigest/getInstance "MD5"))
    (BigInteger. 1)
    (format "%032x")))

(defn find_leading [n leading]
  (if (every? #(= \0 %) (subs (md5 (str "ckczppom" n)) 0 leading))
    n
    (recur (inc n) leading)))

(->> 5 (find_leading 0) (println "Part one:"))
(->> 6 (find_leading 0) (println "Part two:"))
