(ns vf.core-test
  (:require [clojure.test :as t]
            [vf.core :as vf]))


(t/deftest fv-test
  (def pat (vf/fv ["v" (vf/k :major vf/i) \. (vf/k :minor vf/i)]))
  (def data {:major 1 :minor 12})
  (def s "v1.12")

  (t/is (= s (vf/format pat data)))
  (t/is (= data (vf/extract pat s))))


(t/deftest protocol-impl-test
  (t/testing "string constant"
    (t/is (= "v"         (vf/get        "v" nil)))
    (t/is (= "v"         (vf/ensure-ok! "v" "v")))
    (t/is (= "v"         (vf/to-str     "v" "v")))
    (t/is (= "(\\Qv\\E)" (vf/regex      "v")))
    (t/is (= "v"         (vf/parse      "v" "v")))
    (t/is (= {}          (vf/put        "v" {} "v"))))

  (t/testing "char constant"
    (t/is (= \v          (vf/get        \v nil)))
    (t/is (= \v          (vf/ensure-ok! \v \v)))
    (t/is (= "v"         (vf/to-str     \v \v)))
    (t/is (= "(\\Qv\\E)" (vf/regex      \v)))
    (t/is (= \v          (vf/parse      \v "v")))
    (t/is (= {}          (vf/put        \v {} \v))))

  (t/testing "string"
    (t/is (= "v"    (vf/ensure-ok! vf/s "v")))
    (t/is (thrown? java.lang.IllegalArgumentException (vf/ensure-ok! vf/s 1)))
    (t/is (= "v"    (vf/to-str     vf/s "v")))
    (t/is (= "(.*)" (vf/regex      vf/s)))
    (t/is (= "v"    (vf/parse      vf/s "v"))))

  (t/testing "integer"
    (t/is (= 1        (vf/ensure-ok! vf/i 1)))
    (t/is (thrown? java.lang.IllegalArgumentException (vf/ensure-ok! vf/i 1.1)))
    (t/is (= "1"      (vf/to-str     vf/i 1)))
    (t/is (= "(\\d*)" (vf/regex      vf/i)))
    (t/is (= 1        (vf/parse      vf/i "1"))))

  (t/testing "float"
    (t/is (= 1.1                  (vf/ensure-ok! vf/f 1.1)))
    (t/is (thrown? java.lang.IllegalArgumentException (vf/ensure-ok! vf/f 1)))
    (t/is (= "1.1"                (vf/to-str     vf/f 1.1)))
    (t/is (= "(\\d*(?:\\.\\d*)?)" (vf/regex      vf/f)))
    (t/is (= 1.1                  (vf/parse      vf/f "1.1"))))

  (t/testing "key"
    (t/is (= 1        (vf/get        (vf/k :v vf/i) {:v 1})))
    (t/is (= 1        (vf/ensure-ok! (vf/k :v vf/i) 1)))
    (t/is (thrown? java.lang.IllegalArgumentException (vf/ensure-ok! (vf/k :v vf/i) 1.1)))
    (t/is (= "1"      (vf/to-str     (vf/k :v vf/i) 1)))
    (t/is (= "(\\d*)" (vf/regex      (vf/k :v vf/i))))
    (t/is (= 1        (vf/parse      (vf/k :v vf/i) "1")))
    (t/is (= {:v 1}   (vf/put        (vf/k :v vf/i) {} 1))))

  (t/testing "format vector"
    (def pat (vf/fv ["ver" (vf/k :major vf/i) \. (vf/k :minor vf/i)]))
    (def data {:major 1 :minor 12})

    (t/is (= ["ver" 1 \. 12]                    (vf/get        pat data)))
    (t/is (= ["ver" 1 \. 12]                    (vf/ensure-ok! pat ["ver" 1 \. 12])))
    (t/is (thrown? java.lang.IllegalArgumentException (vf/ensure-ok! pat ["ver" "1" \. 12])))
    (t/is (= "ver1.12"                          (vf/to-str     pat ["ver" 1 \. 12])))
    (t/is (= "(\\Qver\\E)(\\d*)(\\Q.\\E)(\\d*)" (vf/regex      pat)))
    (t/is (= ["ver" 1 \. 12]                    (vf/parse      pat ["ver" "1" "." "12"])))
    (t/is (= {:major 1 :minor 12}               (vf/put        pat {} ["ver" 1 \. 12])))))
