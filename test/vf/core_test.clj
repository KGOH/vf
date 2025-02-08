(ns vf.core-test
  (:require [clojure.test :as t]
            [vf.core :as vf]))


(t/deftest vfmt-test
  (def pat (vf/v ["v" (vf/i :major) \. (vf/i :minor)]))
  (def data {:major 1 :minor 12})
  (def s "v1.12")

  (t/is (= s (vf/format pat data)))
  (t/is (= data (vf/extract pat s))))


(t/deftest protocol-impl-test
  (t/testing "string constant"
    (t/is (= "v"         (vf/get    "v" nil)))
    (t/is (= "v"         (vf/to-str "v" "v")))
    (t/is (= "(\\Qv\\E)" (vf/regex  "v")))
    (t/is (= "v"         (vf/parse  "v" "v")))
    (t/is (= {}          (vf/put    "v" {} "v"))))

  (t/testing "char constant"
    (t/is (= \v          (vf/get    \v nil)))
    (t/is (= "v"         (vf/to-str \v \v)))
    (t/is (= "(\\Qv\\E)" (vf/regex  \v)))
    (t/is (= \v          (vf/parse  \v "v")))
    (t/is (= {}          (vf/put    \v {} \v))))

  (t/testing "get from data"
    (t/testing "string"
      (t/is (= "v"      (vf/get    (vf/s :a) {:a "v"})))
      (t/is (= "v"      (vf/to-str (vf/s :a) "v")))
      (t/is (= "(.*)"   (vf/regex  (vf/s :a))))
      (t/is (= "v"      (vf/parse  (vf/s :a) "v")))
      (t/is (= {:a "v"} (vf/put    (vf/s :a) {} "v")))

      (t/testing "non-string values are not accepted"
        (t/is (thrown? java.lang.IllegalArgumentException
                (vf/get (vf/s :a) {:a 1})))))

    (t/testing "casted to a string"
      (t/is (= 1        (vf/get    (vf/as-s :a) {:a 1})))
      (t/is (= "1"      (vf/to-str (vf/as-s :a) 1)))
      (t/is (= "(.*)"   (vf/regex  (vf/as-s :a))))
      (t/is (= "1"      (vf/parse  (vf/as-s :a) "1")))
      (t/is (= {:a "1"} (vf/put    (vf/as-s :a) {} "1"))))

    (t/testing "integer"
      (t/is (= 1        (vf/get    (vf/i :a) {:a 1})))
      (t/is (= "1"      (vf/to-str (vf/i :a) 1)))
      (t/is (= "(\\d*)" (vf/regex  (vf/i :a))))
      (t/is (= 1        (vf/parse  (vf/i :a) "1")))
      (t/is (= {:a 1}   (vf/put    (vf/i :a) {} 1)))

      (t/testing "non-integer values are not accepted"
        (t/is (thrown? java.lang.IllegalArgumentException
                (vf/get (vf/s :a) {:a 1})))))

    (t/testing "float"
      (t/is (= 1.1                  (vf/get    (vf/f :a) {:a 1.1})))
      (t/is (= "1.1"                (vf/to-str (vf/f :a) 1.1)))
      (t/is (= "(\\d*(?:\\.\\d*)?)" (vf/regex  (vf/f :a))))
      (t/is (= 1.1                  (vf/parse  (vf/f :a) "1.1")))
      (t/is (= {:a 1.1}             (vf/put    (vf/f :a) {} 1.1)))

      (t/testing "non-float values are not accepted"
        (t/is (thrown? java.lang.IllegalArgumentException
                (vf/get (vf/s :a) {:a 1}))))))

  (t/testing "vfmt"
    (def pat (vf/v ["ver" (vf/i :major) \. (vf/i :minor)]))
    (def data {:major 1 :minor 12})

    (t/is (= ["ver" 1 \. 12]                    (vf/get pat data)))
    (t/is (= "ver1.12"                          (vf/to-str pat ["ver" 1 \. 12])))
    (t/is (= "(\\Qver\\E)(\\d*)(\\Q.\\E)(\\d*)" (vf/regex pat)))
    (t/is (= ["ver" 1 \. 12]                    (vf/parse pat ["ver" "1" "." "12"])))
    (t/is (= {:major 1 :minor 12}               (vf/put pat {} ["ver" 1 \. 12])))))
