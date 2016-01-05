(defn equal? [x y] ({y true} x false))
(defn not-equal? [x y] ({y false} x true))

(defmacro IF
  ([test then]      `(IF ~test ~then nil))
  ([test then else] `(eval ({false '~else, nil '~else} ~test '~then))))

;(not-equal? 21 20)
;(IF (== 2 3) (+ 2 3) (- 2 3))

(def shape2func
  {:circle    (fn [r] (* r r Math/PI))
   :rectangle (fn [a b] (* a b))
   :triangle  (fn [a h] (* a h 0.5))})

(defn area [shape & args]
  (apply (shape2func shape) args))

(area :rectangle 2 3)