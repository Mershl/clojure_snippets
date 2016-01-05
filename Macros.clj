;(defmacro sq [x] `(let [x# ~x] (* x# x#)))

;(macroexpand '(sq (+ 2 3)))

(defmacro sq [x] (let [x« x] (list '* x« x«)))

;(sq (+ 2 3))