(ns vf.core
  (:refer-clojure :exclude [get format])
  (:require [clojure.edn :as edn]))


(defprotocol vf
  (get    [this data])
  (to-str [this data])
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
  (get    [this _data]  this)
  (to-str [_this value] value)
  (regex  [this]        (str \( (re-quote this) \)))
  (parse  [_ s]         s)
  (put    [_ acc _s]    acc)

  Character
  (get    [this _data]   this)
  (to-str [_this value]  (str value))
  (regex  [this]         (str \( (re-quote (str this)) \)))
  (parse  [_ s]          (first s))
  (put    [_ acc _s]     acc))


(defn s [k]
  (let [p (reify vf
            (get    [this data] (ensure-value-ok! this (clojure.core/get data k) string?))
            (to-str [_ data]    (str data))
            (regex  [_]         "(.*)")
            (parse  [_ s]       s)
            (put    [_ acc x]   (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#vf/s ")
      (print-simple k w))
    p))


(defn as-s [k]
  (let [p (reify vf
            (get    [_ data]  (clojure.core/get data k))
            (to-str [_ data]  (str data))
            (regex  [_]       "(.*)")
            (parse  [_ s]     s)
            (put    [_ acc x] (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#vf/as-s ")
      (print-simple k w))
    p))


(defn i [k]
  (let [p (reify vf
            (get    [this data] (ensure-value-ok! this (clojure.core/get data k) integer?))
            (to-str [_ data]    (str data))
            (regex  [_]         "(\\d*)")
            (parse  [_ s]       (edn/read-string s))
            (put    [_ acc x]   (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#vf/i ")
      (print-simple k w))
    p))


(defn f [k]
  (let [p (reify vf
            (get    [this data] (ensure-value-ok! this (clojure.core/get data k) float?))
            (to-str [_ data]    (str data))
            (regex  [_]         "(\\d*(?:\\.\\d*)?)")
            (parse  [_ s]       (edn/read-string s))
            (put    [_ acc x]   (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#vf/f ")
      (print-simple k w))
    p))


(defn fv [vfmt]
  (let [p (reify vf
            (get    [_ data]   (map #(get % data) vfmt))
            (to-str [_ data]   (apply str (map to-str vfmt data)))
            (regex  [_]        (apply str (map regex vfmt)))
            (parse  [_ ss]     (map parse vfmt ss))
            (put    [_ acc xs] (reduce (fn [acc [that x]] (put that acc x))
                                       acc
                                       (map vector vfmt xs))))]
    (defmethod print-method (type p) [_this w]
      (.write w "#vf/fv")
      (print-simple vfmt w))
    p))
