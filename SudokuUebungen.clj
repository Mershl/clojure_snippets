(range 1 51)

(filter #(not (zero? (mod % 2))) (range 1 51))

(defn toFahrenheit [x]
  (+ (/ (* x 9) 5.0) 32))

(defn createCelsiusFahrenheitList
  ([max] (createCelsiusFahrenheitList 0 max))
  ([min max] (let [celsList (range min (inc max))
                   fahrList (map toFahrenheit celsList)]
               (zipmap celsList fahrList)
               ;(for [x celsList] [x (toFahrenheit x)])
             )
  )
)

;(createCelsiusFahrenheitList 11)

(defn checkPythagoreischTripel [x y z]
  (if (= (+ (* x x) (* y y)) (* z z))
    [x y z]
    nil
  )
)

(defn phytagoreischTripelList
  ([max] (phytagoreischTripelList 0 max))
  ([min max] (
               (let [s (range min (inc max))]
                 (for [a s
                       b s
                       c s
                       :when ((checkPythagoreischTripel a b c))]
                   [a b c]
                 )
               )
  ))
)