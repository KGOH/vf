(ns vf.core
  (:refer-clojure :exclude [get format])
  (:require [clojure.edn :as edn]
            [vf.protocols :as p]))


;; TODO:
;; composable formatters and style fn vs data:
;; key
;;      (vf/fv ["v" (vf/i :major)])
;;      (vf/fv ["v" (vf/key :major vf/i)])
;; (vf/tpl [:fv "v" [:key :major :i]])
;; (vf/tpl [:fv "v" [:major :i]])
;; idx
;;      (vf/fv ["v" (vf/i 0)])
;;      (vf/fv ["v" (vf/idx 0 vf/i)])
;; (vf/tpl [:fv "v" [:idx 0 :i]])
;; (vf/tpl [:fv "v" [:i 0]])
;; pos
;;      (vf/fv ["v" (vf/i)])
;; (vf/tpl [:fv "v" :i])


;;; protocol access ;;;


(defn get    [pat data]  (p/get    pat data))
(defn to-str [pat x]     (p/to-str pat x))
(defn regex  [pat]       (p/regex  pat))
(defn parse  [pat s]     (p/parse  pat s))
(defn put    [pat acc x] (p/put    pat acc x))


;;; public functions ;;;


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


(extend-protocol p/vf
  String
  (get    [this _data] this)
  (to-str [_ x]        x)
  (regex  [this]       (str \( (re-quote this) \)))
  (parse  [_ s]        s)
  (put    [_ acc _x]   acc)

  Character
  (get    [this _data] this)
  (to-str [_ x]        (str x))
  (regex  [this]       (str \( (re-quote (str this)) \)))
  (parse  [_ s]        (first s))
  (put    [_ acc _x]   acc))


(defn ensure-value-ok! [this v value-check-fn]
  (if (value-check-fn v)
    v
    (throw (java.lang.IllegalArgumentException. (str "Unexpected value " (pr-str v) " for " (pr-str this))))))


(defmethod print-method ::vf [this w]
   (.write w "#vf/")
   (.write w (name (:name (meta this))))
   (.write w " ")
   (print-simple (:v (meta this)) w))


(defn s [k]
  ^{:type ::vf, :name :s, :v k}
  (reify
    Object
    (toString [this] (pr-str this))

    p/vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) string?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(.*)")
    (parse  [_ s]       s)
    (put    [_ acc x]   (assoc acc k x))))


(defn as-s [k]
  ^{:type ::vf, :name :as-s, :v k}
  (reify
    Object
    (toString [this] (pr-str this))

    p/vf
    (get    [_ data]  (clojure.core/get data k))
    (to-str [_ x]     (str x))
    (regex  [_]       "(.*)")
    (parse  [_ s]     s)
    (put    [_ acc x] (assoc acc k x))))


(defn i [k]
  ^{:type ::vf, :name :i, :v k}
  (reify
    Object
    (toString [this] (pr-str this))

    p/vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) integer?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(\\d*)")
    (parse  [_ s]       (edn/read-string s))
    (put    [_ acc x]   (assoc acc k x))))


(defn f [k]
  ^{:type ::vf, :name :f, :v k}
  (reify
    Object
    (toString [this] (pr-str this))

    p/vf
    (get    [this data] (ensure-value-ok! this (clojure.core/get data k) float?))
    (to-str [_ x]       (str x))
    (regex  [_]         "(\\d*(?:\\.\\d*)?)")
    (parse  [_ s]       (edn/read-string s))
    (put    [_ acc x]   (assoc acc k x))))


(defn fv [vfmt]
  (let [full-regex (apply str (map regex vfmt))]
    ^{:type ::vf, :name :fv, :v vfmt}
    (reify
      Object
      (toString [this] (pr-str this))

      p/vf
      (get    [_ data]   (map #(get % data) vfmt))
      (to-str [_ xs]     (apply str (map to-str vfmt xs)))
      (regex  [_]        full-regex)
      (parse  [_ ss]     (map parse vfmt ss))
      (put    [_ acc xs] (reduce (fn [acc [that x]] (put that acc x))
                                 acc
                                 (map vector vfmt xs))))))
