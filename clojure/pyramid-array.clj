(ns kata.pyramid)
(defn pyramid [n]
  (vec
    (for [x (range 0 n)]
      (vec (repeat (inc x) 1))
    )
  )
)

(print (pyramid 3))