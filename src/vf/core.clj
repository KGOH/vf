(ns vf.core
  (:refer-clojure :exclude [get format])
  (:require [clojure.edn :as edn]))


(defprotocol vf
  (get    [this data])
  (to-str [this x])
  (regex  [this])
  (parse  [this s])
  (put    [this acc x]))


(defn format [fmt data]
  (->> data
       (get fmt)
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
    (throw (java.lang.IllegalArgumentException. (str "Unexpected value " (pr-str v) " for " (pr-str this))))))


(extend-protocol vf
  String
  (get    [this _data] this)
  (to-str [_this x]    x)
  (regex  [this]       (str \( (re-quote this) \)))
  (parse  [_ s]        s)
  (put    [_ acc _x]   acc)

  Character
  (get    [this _data] this)
  (to-str [_this x]    (str x))
  (regex  [this]       (str \( (re-quote (str this)) \)))
  (parse  [_ s]        (first s))
  (put    [_ acc _x]   acc))


(defmethod print-method ::with-key [this w]
   (.write w "#vf/")
   (.write w (name (:name (meta this))))
   (.write w " ")
   (print-simple (:k (meta this)) w))


(defn s [k]
  ^{:type ::with-key :k k :name :s}
  (reify vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) string?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(.*)")
    (parse  [_ s]       s)
    (put    [_ acc x]   (assoc acc k x))))


(defn as-s [k]
  ^{:type ::with-key :k k :name :as-s}
  (reify vf
    (get    [_ data]  (clojure.core/get data k))
    (to-str [_ x]     (str x))
    (regex  [_]       "(.*)")
    (parse  [_ s]     s)
    (put    [_ acc x] (assoc acc k x))))


(defn i [k]
  ^{:type ::with-key :k k :name :i}
  (reify vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) integer?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(\\d*)")
    (parse  [_ s]       (edn/read-string s))
    (put    [_ acc x]   (assoc acc k x))))


(defn f [k]
  ^{:type ::with-key :k k :name :f}
  (reify vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) float?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(\\d*(?:\\.\\d*)?)")
    (parse  [_ s]       (edn/read-string s))
    (put    [_ acc x]   (assoc acc k x))))


(defmethod print-method ::fv [this w]
  (.write w "#vf/fv")
  (print-simple (:vfmt (meta this)) w))


(defn fv [vfmt]
  ^{:type ::fv :vfmt vfmt}
  (reify vf
    (get    [_ data]   (map #(get % data) vfmt))
    (to-str [_ xs]     (apply str (map to-str vfmt xs)))
    (regex  [_]        (apply str (map regex vfmt)))
    (parse  [_ ss]     (map parse vfmt ss))
    (put    [_ acc xs] (reduce (fn [acc [that x]] (put that acc x))
                               acc
                               (map vector vfmt xs)))))
