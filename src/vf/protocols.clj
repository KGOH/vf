(ns vf.protocols
  (:refer-clojure :exclude [get format]))


(defprotocol Token
  :extend-via-metadata true
  (ok?    [this])
  (to-str [this])
  (regex  [this])
  (parse  [this]))


(defprotocol GetPut
  :extend-via-metadata true
  (get [this data])
  (put [this acc x])
  (token [this]))
