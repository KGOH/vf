(ns vf.core
  (:refer-clojure :exclude [get format key])
  (:require [clojure.edn :as edn]
            [vf.protocols :as p]))


;;; protocol access ;;;


(defn ok?    [pat x] ((p/ok-fn? pat) x))
(defn to-str [pat x] ((p/to-str-fn pat) x))
(defn regex  [pat]   (p/regex pat))
(defn parse  [pat s] ((p/parse-fn pat) s))

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
       (ensure-value-ok! fmt (p/ok-fn? fmt))
       ((p/to-str-fn fmt))))


(defn match [fmt s]
  (-> (p/regex fmt)
      re-pattern
      (re-matches s)
      rest))


(defn extract [fmt s]
  (->> s
       (match fmt)
       ((p/parse-fn fmt))
       (p/put fmt {})))


;;; impl ;;;


(def re-quote java.util.regex.Pattern/quote)


(extend-type String
  p/Token
  (get [this _data]   this)
  (put [_this acc _x] acc)

  (ok-fn?    [_]    identity)
  (to-str-fn [_]    identity)
  (regex     [this] (str \( (re-quote this) \)))
  (parse-fn  [_]    identity))


(extend-type Character
  p/Token
  (get [this _data]   this)
  (put [_this acc _x] acc)

  (ok-fn?    [_]    identity)
  (to-str-fn [_]    str)
  (regex     [this] (str \( (re-quote (str this)) \)))
  (parse-fn  [_]    first))


(extend-type clojure.lang.Keyword
  p/Token
  (get [k data]  (clojure.core/get data k))
  (put [k acc x] (assoc acc k x))

  (ok-fn?    [_] string?)
  (to-str-fn [_] str)
  (regex     [_] "(.*)")
  (parse-fn  [_] identity))


(defn s "string" [k]
  (reify
    p/Token
    (get [_ data]  (clojure.core/get data k))
    (put [_ acc x] (assoc acc k x))

    (ok-fn?    [_] string?)
    (to-str-fn [_] str)
    (regex     [_] "(.*)")
    (parse-fn  [_] identity)))


(defn i "integer" [k]
  (reify
    p/Token
    (get [_ data]  (clojure.core/get data k))
    (put [_ acc x] (assoc acc k x))

    (ok-fn?    [_] integer?)
    (to-str-fn [_] str)
    (regex     [_] "(\\d*)")
    (parse-fn  [_] edn/read-string)))


(defn f "float" [k]
  (reify
    p/Token
    (get [_ data]  (clojure.core/get data k))
    (put [_ acc x] (assoc acc k x))

    (ok-fn?    [_] float?)
    (to-str-fn [_] str)
    (regex     [_] "(\\d*(?:\\.\\d*)?)")
    (parse-fn  [_] edn/read-string)))


(defn fv "format vector" [vfmt]
  (let [to-str-fns  (mapv p/to-str-fn vfmt)
        full-to-str (fn [xs] (apply str (mapv #(%1 %2) to-str-fns xs)))

        full-regex  (apply str (mapv p/regex vfmt))

        parse-fns   (mapv p/parse-fn vfmt)
        full-parse  (fn [xs] (mapv #(%1 %2) parse-fns xs))

        ok-fns      (mapv p/ok-fn? vfmt)
        full-ok?    (fn [xs] (every? boolean (mapv #(%1 %2) ok-fns xs)))]
    (reify
      p/Token
      (get [_ data]   (mapv #(p/get % data) vfmt))
      (put [_ acc xs] (reduce (fn [acc [that x]] (p/put that acc x))
                              acc
                              (mapv vector vfmt xs)))

      (ok-fn?    [_] full-ok?)
      (to-str-fn [_] full-to-str)
      (regex     [_] full-regex)
      (parse-fn  [_] full-parse))))
