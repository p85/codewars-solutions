(defn sum-consecutives [a]
  (def lastv (first a))
  (def newlst ())
  (def tmp (first a))
  (doseq [i (range 0 (count a))]
    (def current (nth a i))
    (def hasNext (contains? a (+ i 1)))
    (if (= hasNext true)
      (def nextV (nth a (+ i 1)))
      (def nextV 0))
    (if (= current nextV)
      (def tmp (+ tmp current))
      (do
        (def newlst (conj newlst tmp))
        (def tmp nextV))))
  (reverse newlst))
(sum-consecutives [1 4 4 4 0 4 3 3 1])