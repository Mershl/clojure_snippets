(defn cqs
    ([coll]
        (cqs coll <)
    )

    ([coll pred]
	(if (empty? coll)
            coll
            (let [pivot (first coll)
                  left  (filter #(pred % pivot) (rest coll))
                  right (remove #(pred % pivot) (rest coll))]
                (concat (cqs left pred) [pivot] (cqs right pred))
            )
        )
     )
)

(cqs (list 2 1 3))