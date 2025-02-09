(ns vf.core
  (:refer-clojure :exclude [get format key])
  (:require [clojure.edn :as edn]
            [vf.protocols :as p]))


;;; protocol access ;;;


(defn ensure-ok! [pat x] (p/ensure-ok! pat x))
(defn to-str     [pat x] (p/to-str pat x))
(defn regex      [pat]   (p/regex  pat))
(defn parse      [pat s] (p/parse  pat s))

(defn get [pat data]  (p/get pat data))
(defn put [pat acc x] (p/put pat acc x))


;;; public functions ;;;


(defn format [fmt data]
  (->> data
       (get fmt)
       (ensure-ok! fmt)
       (to-str fmt)))


(defn match [fmt s]
  (-> (regex fmt)
      re-pattern
      (re-matches s)
      rest))


(defn extract [fmt s]
  (->> s
       (match fmt)
       (parse fmt)
       (put fmt {})))


;;; impl ;;;


(def re-quote java.util.regex.Pattern/quote)


(defn ensure-value-ok! [this v value-check-fn]
  (if (value-check-fn v)
    v
    (throw (java.lang.IllegalArgumentException.
             (str "Unexpected value " (pr-str v) " for " (pr-str this))))))


(extend-type Object
  p/Token
  (ensure-ok! [this x] (-> (p/token this) (p/ensure-ok! x)))
  (to-str     [this x] (-> (p/token this) (p/to-str x)))
  (regex      [this]   (-> (p/token this) p/regex))
  (parse      [this s] (-> (p/token this) (p/parse s))))


(extend-type String
  p/GetPut
  (get [this _data] this)
  (put [_this acc _x] acc)

  p/Token
  (ensure-ok! [_ x]  x)
  (to-str     [_ x]  x)
  (regex      [this] (str \( (re-quote this) \)))
  (parse      [_ s]  s))


(extend-type Character
  p/GetPut
  (get [this _data] this)
  (put [_this acc _x] acc)

  p/Token
  (ensure-ok! [_ x]  x)
  (to-str     [_ x]  (str x))
  (regex      [this] (str \( (re-quote (str this)) \)))
  (parse      [_ s]  (first s)))


(defn k "key" [k token]
  (reify
    p/GetPut
    (get   [_ data]  (clojure.core/get data k))
    (put   [_ acc x] (assoc acc k x))
    (token [_]       token)))


(def s "string"
  (reify p/Token
    (ensure-ok! [this x] (ensure-value-ok! this x string?))
    (to-str     [_ x]    (str x))
    (regex      [_]      "(.*)")
    (parse      [_ s]    s)))


(def i "integer"
  (reify p/Token
    (ensure-ok! [this x] (ensure-value-ok! this x integer?))
    (to-str     [_ x]    (str x))
    (regex      [_]      "(\\d*)")
    (parse      [_ s]    (edn/read-string s))))


(def f "float"
  (reify p/Token
    (ensure-ok! [this x] (ensure-value-ok! this x float?))
    (to-str     [_ x]    (str x))
    (regex      [_]      "(\\d*(?:\\.\\d*)?)")
    (parse      [_ s]    (edn/read-string s))))


(defn fv "format vector" [vfmt]
  (let [full-regex (apply str (map p/regex vfmt))]
    (reify
      p/GetPut
      (get [_ data]   (map #(p/get % data) vfmt))
      (put [_ acc xs] (reduce (fn [acc [that x]] (p/put that acc x))
                              acc
                              (map vector vfmt xs)))

      p/Token
      (to-str     [_ xs] (apply str (map p/to-str vfmt xs)))
      (regex      [_]    full-regex)
      (parse      [_ ss] (map p/parse vfmt ss))
      (ensure-ok! [_ xs] (every? (fn [[that x]] (p/ensure-ok! that x))
                                 (map vector vfmt xs))
                         xs))))
