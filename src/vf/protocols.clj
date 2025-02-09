(ns vf.protocols
  (:refer-clojure :exclude [get format]))


(defprotocol Token
  :extend-via-metadata true
  (ensure-ok! [this x])
  (to-str     [this x])
  (regex      [this])
  (parse      [this s]))


(defprotocol GetPut
  :extend-via-metadata true
  (get [this data])
  (put [this acc x])
  (token [this]))
