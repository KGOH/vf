(ns vf.core
  (:refer-clojure :exclude [get format key])
  (:require [clojure.edn :as edn]
            [vf.protocols :as p]))


;;; protocol access ;;;


(defn ok?    [pat x] ((p/ok? pat) x))
(defn to-str [pat x] ((p/to-str pat) x))
(defn regex  [pat]   (p/regex pat))
(defn parse  [pat s] ((p/parse pat) s))

(defn get [pat data]  (p/get pat data))
(defn put [pat acc x] (p/put pat acc x))


;;; util ;;;

(defn- ensure-value-ok! [this value-check-fn v]
  (if (value-check-fn v)
    v
    (throw (java.lang.IllegalArgumentException.
             (str "Unexpected value " (pr-str v) " for " (pr-str this))))))

;;; public functions ;;;


(defn format [fmt data]
  (->> data
       (p/get fmt)
       (ensure-value-ok! fmt (p/ok? fmt))
       ((p/to-str fmt))))


(defn match [fmt s]
  (-> (p/regex fmt)
      re-pattern
      (re-matches s)
      rest))


(defn extract [fmt s]
  (->> s
       (match fmt)
       ((p/parse fmt))
       (p/put fmt {})))


;;; impl ;;;


(def re-quote java.util.regex.Pattern/quote)




(extend-type Object
  p/Token
  (ok?    [this] (-> (p/token this) p/ok?))
  (to-str [this] (-> (p/token this) p/to-str))
  (regex  [this] (-> (p/token this) p/regex))
  (parse  [this] (-> (p/token this) p/parse)))


(extend-type String
  p/GetPut
  (get [this _data]   this)
  (put [_this acc _x] acc)

  p/Token
  (ok?    [_]    identity)
  (to-str [_]    identity)
  (regex  [this] (str \( (re-quote this) \)))
  (parse  [_]    identity))


(extend-type Character
  p/GetPut
  (get [this _data]   this)
  (put [_this acc _x] acc)

  p/Token
  (ok?    [_]    identity)
  (to-str [_]    str)
  (regex  [this] (str \( (re-quote (str this)) \)))
  (parse  [_]    first))


(defn k "key" [k token]
  (reify
    p/GetPut
    (get   [_ data]  (clojure.core/get data k))
    (put   [_ acc x] (assoc acc k x))
    (token [_]       token)))


(def s "string"
  (reify p/Token
    (ok?    [_] string?)
    (to-str [_] str)
    (regex  [_] "(.*)")
    (parse  [_] identity)))


(def i "integer"
  (reify p/Token
    (ok?    [_] integer?)
    (to-str [_] str)
    (regex  [_] "(\\d*)")
    (parse  [_] edn/read-string)))


(def f "float"
  (reify p/Token
    (ok?    [_] float?)
    (to-str [_] str)
    (regex  [_] "(\\d*(?:\\.\\d*)?)")
    (parse  [_] edn/read-string)))


(defn fv "format vector" [vfmt]
  (let [to-str-fns  (mapv p/to-str vfmt)
        full-to-str (fn [xs] (apply str (mapv #(%1 %2) to-str-fns xs)))

        full-regex  (apply str (mapv p/regex vfmt))

        parse-fns   (mapv p/parse vfmt)
        full-parse  (fn [xs] (mapv #(%1 %2) parse-fns xs))

        ok-fns      (mapv p/ok? vfmt)
        full-ok?    (fn [xs] (every? boolean (mapv #(%1 %2) ok-fns xs)))]
    (reify
      p/GetPut
      (get [_ data]   (mapv #(p/get % data) vfmt))
      (put [_ acc xs] (reduce (fn [acc [that x]] (p/put that acc x))
                              acc
                              (mapv vector vfmt xs)))

      p/Token
      (to-str [_] full-to-str)
      (regex  [_] full-regex)
      (parse  [_] full-parse)
      (ok?    [_] full-ok?))))
