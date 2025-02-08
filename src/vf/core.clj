(ns vf.core
  (:refer-clojure :exclude [get format])
  (:require [clojure.edn :as edn]))


(defprotocol vf
  (get    [this data])
  (to-str [this data])
  (regex  [this])
  (parse  [this s])
  (put    [this s x]))


(defn format [this data]
  (->> data (get this) (to-str this)))


(defn match [this s]
  (rest (re-matches (re-pattern (regex this)) s)))


(defn extract [this s]
  (put this {} (parse this (match this s))))


;;; impl ;;;


(def re-quote java.util.regex.Pattern/quote)


(defn ensure-value-ok! [k v value-check-fn]
  (if (value-check-fn v)
    v
    (throw (java.lang.IllegalArgumentException. (str "Unexpected value " v " for " k)))))


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
            (get    [_ data]  (ensure-value-ok! k (clojure.core/get data k) string?))
            (to-str [_ data]  (str data))
            (regex  [_]       "(.*)")
            (parse  [_ s]     s)
            (put    [_ acc x] (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#v/s ")
      (print-simple k w))
    p))


(defn as-s [k]
  (let [p (reify vf
            (get    [_ data]    (clojure.core/get data k))
            (to-str [_ data]    (str data))
            (regex  [_]         "(.*)")
            (parse  [_ s]       s)
            (put    [_ acc x]   (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#v/as-s ")
      (print-simple k w))
    p))


(defn i [k]
  (let [p (reify vf
            (get    [_ data]  (ensure-value-ok! k (clojure.core/get data k) integer?))
            (to-str [_ data]  (str data))
            (regex  [_]       "(\\d*)")
            (parse  [_ s]     (edn/read-string s))
            (put    [_ acc x] (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#v/i ")
      (print-simple k w))
    p))


(defn f [k]
  (let [p (reify vf
            (get    [_ data]  (ensure-value-ok! k (clojure.core/get data k) float?))
            (to-str [_ data]  (str data))
            (regex  [_]       "(\\d*(?:\\.\\d*)?)")
            (parse  [_ s]     (edn/read-string s))
            (put    [_ acc x] (assoc acc k x)))]
    (defmethod print-method (type p) [_this w]
      (.write w "#v/f ")
      (print-simple k w))
    p))


(defn v [vfmt]
  (let [p (reify vf
            (get    [_ data]   (map #(get % data) vfmt))
            (to-str [_ data]   (apply str (map to-str vfmt data)))
            (regex  [_]        (apply str (map regex vfmt)))
            (parse  [_ ss]     (map parse vfmt ss))
            (put    [_ acc xs] (reduce (fn [acc [that x]] (put that acc x))
                                       acc
                                       (map vector vfmt xs))))]
    (defmethod print-method (type p) [_this w]
      (.write w "#v/v")
      (print-simple vfmt w))
    p))
