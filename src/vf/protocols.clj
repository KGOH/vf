(ns vf.protocols
  (:refer-clojure :exclude [get format]))

(defprotocol vf
  (get    [this data])
  (to-str [this x])
  (regex  [this])
  (parse  [this s])
  (put    [this acc x]))
