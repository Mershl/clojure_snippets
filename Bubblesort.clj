(defn fix [f]
    (fn [x]
        (if (= (f x) x)
          x
          (recur (f x))
        )
    )
)

(defn bubble [itms]
    (if (or (empty? itms) (empty? (rest itms)))
        itms
        (let [fst (first itms)
              snd (second itms)
              rst (rest (rest itms))]
                    (if (> fst snd)
                        (cons snd ((fix bubble) (cons fst rst)))
                        (cons fst ((fix bubble) (cons snd rst)))
                    )
        )
    )
)

(defn bs [items]
    ((fix bubble) items)
)

(bs [2 3 1])