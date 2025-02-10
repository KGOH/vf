(ns vf.core-test
  (:require [clojure.test :as t]
            [vf.core :as vf]))


(t/deftest fv-test
  (def pat (vf/fv [:name " v" (vf/i :major) \. (vf/i :minor)]))
  (def data {:name "app" :major 1 :minor 12})
  (def s "app v1.12")

  (t/is (= s (vf/format pat data)))
  (t/is (= data (vf/extract pat s))))


(t/deftest protocol-impl-test
  (t/testing "string constant"
    (t/is (vf/ok? "v" "v"))
    (t/is (= "v"         (vf/get    "v" nil)))
    (t/is (= "v"         (vf/to-str "v" "v")))
    (t/is (= "(\\Qv\\E)" (vf/regex  "v")))
    (t/is (= "v"         (vf/parse  "v" "v")))
    (t/is (= {}          (vf/put    "v" {} "v"))))

  (t/testing "char constant"
    (t/is (vf/ok? \v \v))
    (t/is (= \v          (vf/get    \v nil)))
    (t/is (= "v"         (vf/to-str \v \v)))
    (t/is (= "(\\Qv\\E)" (vf/regex  \v)))
    (t/is (= \v          (vf/parse  \v "v")))
    (t/is (= {}          (vf/put    \v {} \v))))

  (t/testing "string"
    (t/is (vf/ok? (vf/s :k) "v"))
    (t/is (not (vf/ok? (vf/s :k) 1)))
    (t/is (= "v"      (vf/get (vf/s :k) {:k "v"})))
    (t/is (= {:k "v"} (vf/put (vf/s :k) {} "v")))
    (t/is (= "v"      (vf/to-str (vf/s :k) "v")))
    (t/is (= "(.*)"   (vf/regex  (vf/s :k))))
    (t/is (= "v"      (vf/parse  (vf/s :k) "v"))))

  (t/testing "integer"
    (t/is (vf/ok? (vf/i :k) 1))
    (t/is (not (vf/ok? (vf/i :k) 1.1)))
    (t/is (= 1        (vf/get    (vf/i :k) {:k 1})))
    (t/is (= {:k 1}   (vf/put    (vf/i :k) {} 1)))
    (t/is (= "1"      (vf/to-str (vf/i :k) 1)))
    (t/is (= "(\\d*)" (vf/regex  (vf/i :k))))
    (t/is (= 1        (vf/parse  (vf/i :k) "1"))))

  (t/testing "float"
    (t/is (vf/ok? (vf/f :k) 1.1))
    (t/is (not (vf/ok? (vf/f :k) 1)))
    (t/is (= 1.1                  (vf/get    (vf/f :k) {:k 1.1})))
    (t/is (= {:k 1.1}             (vf/put    (vf/f :k) {} 1.1)))
    (t/is (= "1.1"                (vf/to-str (vf/f :k) 1.1)))
    (t/is (= "(\\d*(?:\\.\\d*)?)" (vf/regex  (vf/f :k))))
    (t/is (= 1.1                  (vf/parse  (vf/f :k) "1.1"))))

  (t/testing "format vector"
    (def pat (vf/fv ["ver" (vf/i :major) \. (vf/i :minor)]))
    (def data {:major 1 :minor 12})

    (t/is (vf/ok? pat ["ver" 1 \. 12]))
    (t/is (not (vf/ok? pat ["ver" "1" \. 12])))
    (t/is (= ["ver" 1 \. 12]                    (vf/get    pat data)))
    (t/is (= "ver1.12"                          (vf/to-str pat ["ver" 1 \. 12])))
    (t/is (= "(\\Qver\\E)(\\d*)(\\Q.\\E)(\\d*)" (vf/regex  pat)))
    (t/is (= ["ver" 1 \. 12]                    (vf/parse  pat ["ver" "1" "." "12"])))
    (t/is (= {:major 1 :minor 12}               (vf/put    pat {} ["ver" 1 \. 12])))))
