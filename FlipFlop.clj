(defn nor [x y] (if (and (zero? x) (zero? y)) 1 0))

(defn ff [nq q s r]
    (let [q+ (nor r nq) nq+ (nor s q)]
        [nq+ q+ (partial ff nq+ q+)]
    )
)

(defn ff2 [nq q]
  (fn [s r]
    (let [nq+ (nor s q) q+ (nor r nq)]
      [nq+ q+ (ff2 nq+ q+)]
    )
  )
)

;((last (ff 1 0 1 0)) 1 0)
(ff2 1 0)
;((last ((ff2 1 0) 1 0)) 1 0)