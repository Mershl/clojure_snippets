(defn fib [n]
    (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))
)

(defn fibend
    ([n] (fibend n 0 1))
    ([n counter prev]
        (if (zero? n) counter (recur (- n 1) (+ counter prev) counter)))
)

(fibend 7)