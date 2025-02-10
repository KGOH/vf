(ns vf.protocols
  (:refer-clojure :exclude [get format]))


(defprotocol Token
  :extend-via-metadata true
  (get [this data])
  (put [this acc x])

  (ok-fn?    [this])
  (to-str-fn [this])
  (regex     [this])
  (parse-fn  [this]))
