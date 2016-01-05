(ns de.mweires
  (:use clojure.test))

(defn sorting [x]
    (list x (+ x 1) (+ x 2)))

(defn ordercheck [L]
  (if (empty? L)
      true
      (let [x (first L)
            nex (first (rest L))]
            (if (nil? nex) true)
            (if (< nex x) false (ordercheck (rest L)))
      )
  )
)

(defn correct-qsort
    ([coll]
        (correct-qsort coll <)
    )

    ([coll pred]
	(if (empty? coll)
            coll
            (let [pivot (first coll)
                  left  (filter #(pred % pivot) (rest coll))
                  right (remove #(pred % pivot) (rest coll))]
                (concat (correct-qsort left pred) [pivot] (correct-qsort right pred))
            )
        )
     )
)

(deftest test-sorting
    (is (= (ordercheck (sorting 1)) true))
    (is (= (correct-qsort (sorting 1)) (sorting 1)))
)

(test-sorting)